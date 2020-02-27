# -*- coding: utf-8 -*-
"""
For checking MELPA recipe pull-requests, building docker containers
that run the checks against the package pointed to by the recipe, and
for running a handful of other miscellaneous checks that are easier to
write here than in elisp.

Test this file:
  pytest --doctest-modules

Use this file a script:
  python <this_file>.py
"""
from __future__ import print_function
import functools
import glob
import os
import random
import re
import requests
import subprocess
import sys
import tempfile
import time
from typing import Iterator, Tuple

DEBUG = False  # eagerly load installed packages, etc.

# define the colors of the report (or none), per https://no-color.org
# https://misc.flogisoft.com/bash/tip_colors_and_formatting
NO_COLOR = os.environ.get('NO_COLOR')
CLR_OFF = '' if NO_COLOR else '\033[0m'
CLR_ERR = '' if NO_COLOR else '\033[31m'
CLR_WARN = '' if NO_COLOR else '\033[33m'
CLR_INFO = '' if NO_COLOR else '\033[32m'
CLR_ULINE = '' if NO_COLOR else '\033[4m'

GITHUB_API = 'https://api.github.com/repos'
MELPA_PR = r'https://github.com/melpa/melpa/pull/([0-9]+)'
MELPA_PULL_API = f"{GITHUB_API}/melpa/melpa/pulls"
MELPA_RECIPES = f"{GITHUB_API}/melpa/melpa/contents/recipes"

# Valid licenses and their names according to the GitHub API
# TODO: complete this list!
VALID_LICENSES_GITHUB = {
    'Apache License 2.0',
    'GNU Affero General Public License v3.0',
    'GNU General Public License v2.0',
    'GNU General Public License v3.0',
    'GNU Lesser General Public License v3.0',
    'MIT License',
    'The Unlicense',
}


def return_code(return_code: int = None):
    """Return (and optionally set) the return code."""
    global _RETURN_CODE
    _RETURN_CODE = 0
    if return_code is not None:
        _RETURN_CODE = return_code
    return _RETURN_CODE


def run_checks(
    recipe: str,  # e.g. of the form (shx :repo ...)
    elisp_dir: str,  # where the package is
    clone_address: str = None,  # optional repo address
    pr_data: dict = None,  # optional data from the PR
):
    """Entrypoint for running all checks."""
    return_code(0)
    files: list = _files_in_recipe(recipe, elisp_dir)
    subprocess.check_output(['rm', '-rf', '_elisp'])
    os.makedirs('_elisp')
    for ii, recipe_file in enumerate(files):
        subprocess.check_output(['cp', recipe_file, '_elisp/'])
        files[ii] = os.path.join('_elisp', os.path.basename(recipe_file))
    _write_requirements(files, recipe)
    check_containerized_build(_package_name(recipe))
    check_license(files, elisp_dir, clone_address)
    check_packaging(files, recipe)
    print_related_packages(recipe)  # could throw ConnectionError
    print_details(recipe, files, pr_data, clone_address)


def check_containerized_build(package_name):
    print('Building container... 🐳')
    output = subprocess.check_output(['make', 'test', f"PACKAGE_NAME={package_name}"])
    output = output.decode()
    output_lines = output.strip().split('\n')
    for ii, output_line in enumerate(output_lines):
        # byte-compile-file writes ":Error: ", package-lint ": error: "
        if ':Error: ' in output_line or ': error: ' in output_line:
            output_line = f"{CLR_ERR}{output_line}{CLR_OFF}"
        elif ':Warning: ' in output_line or ': warning: ' in output_line:
            output_line = f"{CLR_WARN}{output_line}{CLR_OFF}"
        elif output_line.startswith('### '):
            output_line = f"{CLR_INFO}{output_line}{CLR_OFF}"
        output_lines[ii] = output_line
    output = '\n'.join(output_lines)
    print(output)
    if CLR_ERR in output:
        return_code(1)


def _files_in_recipe(recipe: str, elisp_dir: str) -> list:
    files: list = subprocess.check_output(['find', elisp_dir]).decode().split()
    recipe_tokens: list = _tokenize_recipe(recipe)
    if ':files' in recipe_tokens:
        files_inc, files_exc = _apply_recipe(recipe, elisp_dir)
    else:
        files_inc, files_exc = _apply_default_recipe(elisp_dir)
    return list(set(files) & set(files_inc) - set(files_exc))


@functools.lru_cache()
def _tokenize_recipe(recipe: str) -> list:
    """
    >>> _tokenize_recipe('(shx :repo "riscy/shx-for-emacs" :fetcher github)')
    ['(', 'shx', ':repo', '"riscy/shx-for-emacs"', ':fetcher', 'github', ')']
    """
    recipe = ' '.join(recipe.split())
    recipe = recipe.replace('(', ' ( ')
    recipe = recipe.replace(')', ' ) ')
    return recipe.split()


def _apply_recipe(recipe: str, elisp_dir: str) -> Tuple[list, list]:
    # TODO: this could possibly use the MELPA machinery instead
    files_inc: list = []
    files_exc: list = []
    scope = None
    nesting = 0
    recipe_tokens = _tokenize_recipe(recipe)
    for token in recipe_tokens[recipe_tokens.index(':files') + 1 :]:
        if token == '(':
            nesting += 1
        elif token == ')':
            scope = None
            nesting -= 1
            if not nesting:
                break
        elif token == ':defaults':
            include, exclude = _apply_default_recipe(elisp_dir)
            files_inc += include
            files_exc += exclude
        elif token == ':exclude':
            scope = token
        elif scope == ':exclude':
            files_exc += glob.glob(os.path.join(elisp_dir, token.strip('"')))
        else:
            files_inc += glob.glob(os.path.join(elisp_dir, token.strip('"')))
    return files_inc, files_exc


def _apply_default_recipe(elisp_dir: str) -> Tuple[list, list]:
    # TODO: this could possibly use the MELPA machinery instead
    files_inc = glob.glob(os.path.abspath(os.path.join(elisp_dir, '*.el')))
    files_exc = (
        glob.glob(os.path.abspath(os.path.join(elisp_dir, 'test.el')))
        + glob.glob(os.path.abspath(os.path.join(elisp_dir, 'tests.el')))
        + glob.glob(os.path.abspath(os.path.join(elisp_dir, '*-test.el')))
        + glob.glob(os.path.abspath(os.path.join(elisp_dir, '*-tests.el')))
    )
    return files_inc, files_exc


@functools.lru_cache()
def _package_name(recipe: str) -> str:
    """
    >>> _package_name('(shx :files ...)')
    'shx'
    """
    return recipe.split()[0].strip('(') if recipe else ''


def _main_file(recipe_files: list, recipe: str) -> str:
    """
    >>> _main_file(['_elisp/a.el', '_elisp/b.el'], '(a :files ...)')
    '_elisp/a.el'
    >>> _main_file(['a.el', 'b.el'], '(b :files ...)')
    'b.el'
    """
    try:
        package_name = _package_name(recipe)
        return next(
            el for el in recipe_files if os.path.basename(el) == f"{package_name}.el"
        )
    except StopIteration:
        return ''


def _write_requirements(recipe_files: list, recipe: str):
    """Create a little elisp script that Docker will run as setup."""
    # TODO: this could possibly use Cask instead
    with open('_requirements.el', 'w') as requirements_el:
        requirements_el.write(
            '''
            (require 'package)
            (package-initialize)
            (setq package-archives nil)
            ;; TODO: is it still necessary to use GNU elpa mirror?
            (add-to-list 'package-archives '("gnu"   . "http://mirrors.163.com/elpa/gnu/"))
            (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
            (add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/"))
            (package-refresh-contents)
            (package-reinstall 'package-lint)
            '''
        )
        for req in _requirements(recipe_files, recipe):
            if req != 'emacs':
                # TODO check if we need to reinstall?
                # e.g. (package-installed-p 'map (version-to-list "2.0"))
                requirements_el.write(f"(package-install '{req})\n")
                if DEBUG:
                    requirements_el.write(f"(require '{req})\n")


def _requirements(
    recipe_files: list, recipe: str = None, with_versions: bool = False
) -> set:
    reqs: list = []
    if recipe:
        main_file = _main_file(recipe_files, recipe)
        if main_file:
            recipe_files = [main_file]
    for filename in recipe_files:
        if not os.path.isfile(filename):
            continue
        try:
            reqs.append(
                subprocess.check_output(
                    f"grep -i 'Package-Requires' {filename}", shell=True
                )
                .decode('utf-8')
                .strip()
            )
        except subprocess.CalledProcessError:
            pass
    reqs = sum([req.split('(')[1:] for req in reqs], [])
    reqs = [req.replace(')', '').strip().lower() for req in reqs if req]
    if with_versions:
        return set(reqs)
    return {req.split('"')[0].strip() for req in reqs}


def check_license(recipe_files: list, elisp_dir: str, clone_address: str = None):
    print('\nLicense:')
    repo_licensed = False
    if clone_address:
        repo_licensed = _check_license_github_api(clone_address)
    if not repo_licensed:
        repo_licensed = _check_license_file(elisp_dir)
    individual_files_licensed = _check_license_in_files(recipe_files)
    if not repo_licensed and not individual_files_licensed:
        print(
            f"- {CLR_ERR}Use a "
            '[GPL-compatible](https://www.gnu.org/licenses/license-list.en.html#GPLCompatibleLicenses)'
            f" license{CLR_OFF}"
        )
        return_code(1)


def _check_license_github_api(clone_address: str) -> bool:
    # TODO: gitlab also has a license API -- support it?
    match = re.search(r'github.com/([^"]*)', clone_address)
    if not match:
        return False
    repo_suffix = match.groups()[0].strip('/')
    license_ = requests.get(f"{GITHUB_API}/{repo_suffix}").json().get('license')
    if license_ and license_.get('name') in VALID_LICENSES_GITHUB:
        print(f"- GitHub API found `{license_.get('name')}`")
        return True
    if license_:
        print(f"- {CLR_WARN}GitHub API found `{license_.get('name')}`{CLR_OFF}")
        if license_.get('name') == 'Other':
            print(
                f"  - {CLR_ERR}Use a [GitHub-compatible](https://github.com/licensee/licensee) format for your license file{CLR_OFF}"
            )
            return_code(1)
        return False
    print(
        f"- {CLR_ERR}Add an [automatically detectable](https://github.com/licensee/licensee) LICENSE file to your repository (e.g. no markup){CLR_OFF}"
    )
    return_code(1)
    return False


def _check_license_file(elisp_dir: str) -> bool:
    """Scan any COPYING or LICENSE files."""
    for license_ in glob.glob(os.path.join(elisp_dir, '*')):
        license_ = os.path.basename(license_)
        if license_.startswith('LICENSE') or license_.startswith('COPYING'):
            with open(os.path.join(elisp_dir, license_)) as stream:
                print(f"- {license_} excerpt: `{stream.readline().strip()}...`")
            return True
    return False


def _check_license_in_files(elisp_files: list) -> bool:
    """Check the elisp files themselves."""
    individual_files_licensed = True
    for elisp_file in elisp_files:
        license_ = _check_license_in_file(elisp_file)
        basename = os.path.basename(elisp_file)
        if not license_:
            print(f"- {CLR_ERR}{basename} has no detectable license text{CLR_OFF}")
            individual_files_licensed = False
        else:
            print(f"- {basename} has {license_} license text")
    return individual_files_licensed


def _check_license_in_file(elisp_file: str) -> str:
    """Scan the elisp file for some recognized license text."""
    # TODO: this function could be more comprehensive
    licenses = {
        'GPL': r'GNU.* General Public License',
        'ISC': r'Permission to use, copy, modify, and/or',
        'MIT': r'Permission is hereby granted, free of charge, to any person',
        'Unlicense': 'This is free and unencumbered software released into the public domain',
    }
    for license_key, license_txt in licenses.items():
        try:
            subprocess.check_output(['grep', '-i', license_txt, elisp_file])
            return license_key
        except subprocess.CalledProcessError:
            pass
    return ''


def check_packaging(recipe_files: list, recipe: str):
    if ':branch "master"' in recipe:
        print('- {CLR_ERR}No need to specify `:branch "master"` in recipe{CLR_OFF}')
        return_code(1)
    if 'gitlab' in recipe and (':repo' not in recipe or ':url' in recipe):
        print(
            '- {CLR_ERR}With the GitLab fetcher you MUST set :repo and you MUST NOT set :url{CLR_OFF}'
        )
        return_code(1)
    # MELPA looks for a -pkg.el file and if it finds it, it uses that. It is
    # okay to have a -pkg.el file, but doing it incorrectly can break the build:
    for pkg_file in (el for el in recipe_files if el.endswith('-pkg.el')):
        print(
            f"- {CLR_ERR}Including {os.path.basename(pkg_file)} is discouraged -- MELPA will create a `-pkg.el` file{CLR_OFF}"
        )
        return_code(1)
    # If it can't find a -pkg.el file, it looks in <your-package-name>.el.  If
    # you put your package info in your main file then we can use package-lint
    # to catch mistakes and enforce consistency.
    if not _main_file(recipe_files, recipe):
        print(
            f"- {CLR_ERR}There is no .el file matching the package name '{_package_name(recipe)}'{CLR_OFF}"
        )
        return_code(1)
    # In fact, if you have different Package-Requires among your source files,
    # the Package-Requires that aren't in <your-package-name>.el are ignored,
    # and there is at least one package in MELPA that accidentally does this.
    all_requirements = set(_requirements(recipe_files, recipe))
    for el in recipe_files:
        el_requirements = set(_requirements([el]))
        if el_requirements and el_requirements != all_requirements:
            print(f"- {CLR_ERR}Package-Requires mismatch between .el files!{CLR_OFF}")
            return_code(1)


def print_details(
    recipe: str, recipe_files: list, pr_data: dict = None, clone_address: str = None
):
    print('\nDetails:')
    print(f"- `{recipe}`")
    if ':files' in recipe:
        print('  - Try to simply use the default recipe, if possible')
    print('- Package-Requires: ', end='')
    if _requirements(recipe_files):
        print(', '.join(req for req in _requirements(recipe_files, with_versions=True)))
    else:
        print('n/a')
    for recipe_file in recipe_files:
        with open(recipe_file) as stream:
            try:
                header = stream.readline()
                header = header.split('-*-')[0]
                header = header.split(' --- ')[1]
                header = header.strip()
            except (IndexError, UnicodeDecodeError):
                header = f"{CLR_ERR}Couldn't parse header{CLR_OFF}"
        print(
            f"- {'📁 ' if os.path.isdir(recipe_file) else ''}"
            f"{CLR_ULINE}{recipe_file}{CLR_OFF}"
            f" ({_check_license_in_file(recipe_file) or 'unknown license'})"
            + (f": {header}" if header else "")
        )
    if pr_data and clone_address:
        # Check the maintainer
        print(f"- PR by {pr_data['user']['login']}: {clone_address}")
        if pr_data['user']['login'].lower() not in clone_address.lower():
            print(f"  - {CLR_WARN}NOTE: Repo and recipe owner don't match{CLR_OFF}")
        if int(pr_data['changed_files']) != 1:
            print(f"  - {CLR_ERR}PR changes {pr_data['changed_files']} files{CLR_OFF}")


def print_related_packages(recipe: str):
    """
    Print list of potentially related packages.
    Could throw ConnectionError.
    """
    # TODO: can this be made more useful?
    package_tokens = {
        token for token in _package_name(recipe).split('-') if token != 'mode'
    }
    related_packages = [
        f"- https://melpa.org/#/{_melpa_archive()[other_package_tokens]}"
        for other_package_tokens in _melpa_archive()
        if package_tokens & other_package_tokens
    ]
    if related_packages:
        print('\nPossibly related packages:')
        print('\n'.join(related_packages[:5]))


@functools.lru_cache()
def _melpa_archive() -> dict:
    return {
        frozenset(package.split('-')): package
        for package in requests.get('http://melpa.org/archive.json').json()
    }


def yes_p(text: str) -> bool:
    while True:
        keep = input(f"{text} [y/n] ").strip().lower()
        if keep.startswith('y') or keep.startswith('n'):
            break
    return not keep.startswith('n')


def check_remote_package(clone_address: str, recipe: str = ''):
    """Check a remotely-hosted package."""
    with tempfile.TemporaryDirectory() as elisp_dir:
        _clone(clone_address, _branch(recipe), into=elisp_dir)
        run_checks(recipe, elisp_dir, clone_address)


def _clone(repo: str, branch: str, into: str):
    print(f"Cloning {repo}")
    subprocess.check_output(['mkdir', '-p', into])
    # git clone prints to stderr, oddly enough:
    try:
        subprocess.check_output(
            ['git', 'clone', '-b', branch, repo, into], stderr=subprocess.STDOUT
        )
    except subprocess.CalledProcessError as err:
        print(f"{CLR_ERR}The default branch is not 'master'{CLR_OFF}")
        if branch == 'master':
            subprocess.check_output(['git', 'clone', repo, into])
            return
        raise err


@functools.lru_cache()
def _branch(recipe: str) -> str:
    """
    >>> _branch('(shx :branch "develop" ...)')
    'develop'
    """
    match = re.search(':branch "([^"]*)"', recipe)
    if match:
        return match.groups()[0]
    return 'master'


def check_local_package(elisp_dir: str = None, package_name: str = None):
    """Check a locally-hosted package."""
    elisp_dir = elisp_dir or input('Path: ').strip()
    assert os.path.isdir(elisp_dir)
    package_name = package_name or input(f"Name of package at {elisp_dir}: ")
    recipe = f'({package_name or "NONAME"} :repo "N/A")'
    run_checks(recipe, elisp_dir)


def check_melpa_pr(pr_url: str):
    """Check a PR on MELPA."""
    match = re.search(MELPA_PR, pr_url)  # MELPA_PR's 0th group has the number
    assert match
    pr_data = requests.get(f"{MELPA_PULL_API}/{match.groups()[0]}").json()
    recipe: str = _recipe(pr_data['diff_url'])
    clone_address: str = _clone_address(pr_data['body'])
    try:
        with tempfile.TemporaryDirectory() as elisp_dir:
            _clone(clone_address, _branch(recipe), into=elisp_dir)
            return run_checks(recipe, elisp_dir, clone_address, pr_data)
    except subprocess.CalledProcessError as err:
        template = 'https://github.com/melpa/melpa/blob/master/.github/PULL_REQUEST_TEMPLATE.md'
        print(f"{CLR_ERR}{err}: is {template} intact?{CLR_OFF}")
        return_code(1)


def _recipe(pr_data_diff_url: str) -> str:
    "Download the user's recipe."
    # TODO: use https://developer.github.com/v3/repos/contents/ instead of 'patch'
    with tempfile.TemporaryDirectory() as elisp_dir:
        try:
            diff_filename = os.path.join(elisp_dir, 'diff')
            recipe_filename = os.path.join(elisp_dir, 'recipe')
            with open(diff_filename, 'w') as diff_file:
                diff_file.write(requests.get(pr_data_diff_url).text)
            subprocess.check_output(
                f"patch {recipe_filename} < {diff_filename}", shell=True
            )
            with open(recipe_filename) as recipe_file:
                recipe = re.sub(r'\s+', ' ', recipe_file.read())
        except subprocess.CalledProcessError:
            print('Recipe read HACK failed.  Using default recipe')
            recipe = ''
        return recipe.strip()


@functools.lru_cache()
def _clone_address(pr_text: str) -> str:
    """Figure out the clone address."""
    url_list = pr_text.split('Direct link to the package repository')[-1].split()
    url = next(url for url in url_list if url.startswith('http'))
    # special handling for some sites:
    if '//launchpad.net' in url:
        url = url.replace('//launchpad.net', '//git.launchpad.net')
    return url


def check_melpa_pr_loop():
    """Check MELPA pull requests in a loop."""
    for pr_url in _fetch_pull_requests():
        print(f"Found MELPA PR {pr_url}")
        check_melpa_pr(pr_url)
        print('-' * 79)


def _fetch_pull_requests() -> Iterator[str]:
    """Repeatedly yield PR URL's."""
    # TODO: only supports macOS (needs pbpaste or equivalents)
    previous_pr_url = None
    while True:
        while True:
            match = re.search(MELPA_PR, subprocess.check_output('pbpaste').decode())
            pr_url = match.string[: match.end()] if match else None
            if match and pr_url and pr_url != previous_pr_url:
                break
            print(
                'Watching clipboard for MELPA PR... '
                + ('😐' if random.randint(0, 2) else '🤨'),
                end='\r',
            )
            time.sleep(1)
        previous_pr_url = pr_url
        yield pr_url


if __name__ == '__main__':
    if 'MELPA_PR_URL' in os.environ:
        check_melpa_pr(os.environ['MELPA_PR_URL'])
        sys.exit(return_code())
    elif 'PKG_PATH' in os.environ and 'PKG_NAME' in os.environ:
        check_local_package(os.environ['PKG_PATH'], os.environ['PKG_NAME'])
        sys.exit(return_code())
    elif 'CLONE_URL' in os.environ:
        if 'RECIPE' in os.environ:
            check_remote_package(os.environ['CLONE_URL'], os.environ['RECIPE'])
            sys.exit(return_code())
        else:
            check_remote_package(os.environ['CLONE_URL'])
            sys.exit(return_code())
    else:
        check_melpa_pr_loop()
