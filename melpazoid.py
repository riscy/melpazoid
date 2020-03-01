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
_RETURN_CODE = 0

# define the colors of the report (or none), per https://no-color.org
# https://misc.flogisoft.com/bash/tip_colors_and_formatting
NO_COLOR = os.environ.get('NO_COLOR')
CLR_OFF = '' if NO_COLOR else '\033[0m'
CLR_ERROR = '' if NO_COLOR else '\033[31m'
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
    'ISC License',
    'MIT License',
    'The Unlicense',
}


def run_checks(
    recipe: str,  # e.g. of the form (shx :repo ...)
    elisp_dir: str,  # where the package is on this machine
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


def return_code(return_code: int = None) -> int:
    """
    Return (and optionally set) the current return code.
    If return_code matches env var EXPECT_ERROR, return 0 --
    this is useful for running CI checks on melpazoid itself.
    """
    global _RETURN_CODE
    if return_code is not None:
        _RETURN_CODE = return_code
    expect_error = int(os.environ.get('EXPECT_ERROR', 0))
    return 0 if _RETURN_CODE == expect_error else _RETURN_CODE


def _note(message: str, color: str = None, highlight: str = None):
    """Print a note, possibly in color, possibly highlighting specific text."""
    color = color or ''
    if highlight:
        print(re.sub(f"({highlight})", f"{color}\\g<1>{CLR_OFF}", message))
    else:
        print(f"{color}{message}{CLR_OFF}")


def _fail(message: str, color: str = CLR_ERROR, highlight: str = None):
    _note(message, color, highlight)
    return_code(2)


def check_containerized_build(package_name):
    print('Building container... 🐳')
    output = subprocess.check_output(['make', 'test', f"PACKAGE_NAME={package_name}"])
    output = output.decode()
    output_lines = output.strip().split('\n')
    for output_line in output_lines:
        # byte-compile-file writes ":Error: ", package-lint ": error: "
        if ':Error: ' in output_line or ': error: ' in output_line:
            _fail(output_line, highlight=r' ?[Ee]rror:')
        elif ':Warning: ' in output_line or ': warning: ' in output_line:
            _note(output_line, CLR_WARN, highlight=r' ?[Ww]arning:')
        elif output_line.startswith('### '):
            _note(output_line, CLR_INFO)
        else:
            print(output_line)


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
    tokenized_recipe: list = recipe.split()
    assert (
        tokenized_recipe[0] == '('
        and tokenized_recipe[-1] == ')'
        and len([pp for pp in tokenized_recipe if pp == '('])
        == len([pp for pp in tokenized_recipe if pp == ')'])
    ), f"Recipe {recipe} doesn't look right"
    return tokenized_recipe


def _apply_recipe(recipe: str, elisp_dir: str) -> Tuple[list, list]:
    # TODO: this could possibly use the MELPA machinery instead
    files_inc: list = []
    files_exc: list = []
    excluding = False
    nesting = 0
    recipe_tokens = _tokenize_recipe(recipe)
    for token in recipe_tokens[recipe_tokens.index(':files') + 1 :]:
        if token == '(':
            nesting += 1
        elif token == ')':
            excluding = False
            nesting -= 1
            if not nesting:
                break
        elif token == ':defaults':
            include, exclude = _apply_default_recipe(elisp_dir)
            files_inc += include
            files_exc += exclude
        elif token == ':exclude':
            excluding = True
        elif excluding:
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
            ;; FIXME: is it still necessary to use GNU elpa mirror?
            (add-to-list 'package-archives '("gnu"   . "http://mirrors.163.com/elpa/gnu/"))
            (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
            (add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/"))
            (package-refresh-contents)
            (package-reinstall 'package-lint)
            '''
        )
        for req in _requirements(recipe_files, recipe):
            if req != 'emacs':
                # TODO check if we need to reinstall outdated package?
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
    _note('\n### License ###\n', CLR_INFO)
    repo_licensed = False
    if clone_address:
        repo_licensed = _check_license_github_api(clone_address)
    if not repo_licensed:
        repo_licensed = _check_license_file(elisp_dir)
    individual_files_licensed = _check_license_in_files(recipe_files)
    if not repo_licensed and not individual_files_licensed:
        _fail('- Use a GPL-compatible license.')
        print(
            '  See: https://www.gnu.org/licenses/license-list.en.html#GPLCompatibleLicenses'
        )


def _check_license_github_api(clone_address: str) -> bool:
    # TODO: gitlab also has a license API -- support it?
    # e.g. https://gitlab.com/api/v4/users/jagrg/projects ?
    match = re.search(r'github.com/([^"]*)', clone_address, flags=re.I)
    if not match:
        return False
    repo_suffix = match.groups()[0].rstrip('.git').strip('/')
    license_ = requests.get(f"{GITHUB_API}/{repo_suffix}").json().get('license')
    if license_ and license_.get('name') in VALID_LICENSES_GITHUB:
        print(f"- GitHub API found `{license_.get('name')}`")
        return True
    if license_:
        _note(f"- GitHub API found `{license_.get('name')}`", CLR_WARN)
        if license_.get('name') == 'Other':
            _fail('  - Use a GitHub-compatible format for your license file.')
            print('    See: https://github.com/licensee/licensee')
        return False
    _fail('- Use a LICENSE file that GitHub can detect (e.g. no markup).')
    print('  See: https://github.com/licensee/licensee')
    return False


def _check_license_file(elisp_dir: str) -> bool:
    """Scan any COPYING or LICENSE files."""
    for license_ in glob.glob(os.path.join(elisp_dir, '*')):
        license_ = os.path.basename(license_)
        if license_.startswith('LICENSE') or license_.startswith('COPYING'):
            with open(os.path.join(elisp_dir, license_)) as stream:
                print(f"- {license_} excerpt: `{stream.readline().strip()}...`")
            return True
    _fail('- No LICENSE or COPYING file found in repository')
    return False


def _check_license_in_files(elisp_files: list) -> bool:
    """Check the elisp files themselves."""
    individual_files_licensed = True
    for elisp_file in elisp_files:
        license_ = _check_license_in_file(elisp_file)
        basename = os.path.basename(elisp_file)
        if not license_:
            _fail(f"- {basename} has no detectable license text")
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
        _fail('- No need to specify `:branch "master"` in recipe')
    if 'gitlab' in recipe and (':repo' not in recipe or ':url' in recipe):
        _fail('- With the GitLab fetcher you MUST set :repo and you MUST NOT set :url')
    # MELPA looks for a -pkg.el file and if it finds it, it uses that. It is
    # okay to have a -pkg.el file, but doing it incorrectly can break the build:
    for pkg_file in (el for el in recipe_files if el.endswith('-pkg.el')):
        pkg_file = os.path.basename(pkg_file)
        _fail(f"- Avoid packaging {pkg_file} -- MELPA creates a `-pkg.el` file")
    # If it can't find a -pkg.el file, it looks in <your-package-name>.el.  If
    # you put your package info in your main file then we can use package-lint
    # to catch mistakes and enforce consistency.
    if not _main_file(recipe_files, recipe):
        package_name = _package_name(recipe)
        _fail(f"- There is no .el file matching the package name '{package_name}'")
    # In fact, if you have different Package-Requires among your source files,
    # the Package-Requires that aren't in <your-package-name>.el are ignored,
    # and there is at least one package in MELPA that accidentally does this.
    all_requirements = set(_requirements(recipe_files, recipe))
    for el in recipe_files:
        el_requirements = set(_requirements([el]))
        if el_requirements and el_requirements != all_requirements:
            _fail(f"- Package-Requires mismatch between .el files!")


def print_details(
    recipe: str, recipe_files: list, pr_data: dict = None, clone_address: str = None
):
    _note('\n### Details ###\n', CLR_INFO)
    print(f"- `{recipe}`")
    if ':files' in recipe:
        _note('  - Try to simply use the default recipe if possible', CLR_WARN)
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
                header = f"{CLR_ERROR}Couldn't parse header{CLR_OFF}"
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
            _note("  - NOTE: Repo and recipe owner don't match", CLR_WARN)


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
        _note('\n### Similarly named packages ###\n', CLR_INFO)
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


def check_remote_package(recipe: str = ''):
    """Check a remotely-hosted package."""
    name = _tokenize_recipe(recipe)[1]
    with tempfile.TemporaryDirectory() as elisp_dir:
        clone_address = _clone_address(name, recipe)
        _clone(clone_address, _branch(recipe), into=elisp_dir)
        run_checks(recipe, elisp_dir, clone_address)


def _clone(repo: str, branch: str, into: str):
    """Raises subprocess.CalledProcessError if git clone fails."""
    print(f"Cloning {repo}")
    subprocess.check_output(['mkdir', '-p', into])
    # git clone prints to stderr, oddly enough:
    subprocess.check_output(
        ['git', 'clone', '-b', branch, repo, into], stderr=subprocess.STDOUT
    )


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
    if int(pr_data['changed_files']) != 1:
        _fail('Please only add one recipe per pull request')
        return

    name, recipe = _name_and_recipe(pr_data['diff_url'])
    if not name or not recipe:
        _fail('Unable to build this pull request.')
        return

    clone_address: str = _clone_address(name, recipe)
    with tempfile.TemporaryDirectory() as elisp_dir:
        _clone(clone_address, _branch(recipe), into=elisp_dir)
        return run_checks(recipe, elisp_dir, clone_address, pr_data)


@functools.lru_cache()
def _name_and_recipe(pr_data_diff_url: str) -> Tuple[str, str]:
    """Determine the filename and the contents of the user's recipe."""
    # TODO: use https://developer.github.com/v3/repos/contents/ instead of 'patch'
    with tempfile.TemporaryDirectory() as tmpdir:
        try:
            diff_text = requests.get(pr_data_diff_url).text
            recipe_name = diff_text.split('\n')[0].split('/')[-1]
            diff_filename = os.path.join(tmpdir, 'diff')
            recipe_filename = os.path.join(tmpdir, recipe_name)
            with open(diff_filename, 'w') as diff_file:
                diff_file.write(diff_text)
            subprocess.check_output(
                f"patch {recipe_filename} < {diff_filename}", shell=True
            )
            with open(recipe_filename) as recipe_file:
                recipe = re.sub(r'\s+', ' ', recipe_file.read())
            return recipe_name, recipe.strip()
        except subprocess.CalledProcessError as err:
            _note(str(err), CLR_WARN)
            return '', ''


def _clone_address(name: str, recipe: str) -> str:
    """
    This is a HACK to get the clone address from the
    filename/recipe pair using the builtin MELPA machinery.  As a
    bonus, it validates the recipe.
    >>> _clone_address('shx', '(shx :repo "riscy/shx-for-emacs" :fetcher github)')
    'https://github.com/riscy/shx-for-emacs.git'
    >>> _clone_address('pmdm', '(pmdm :fetcher hg :url "https://hg.serna.eu/emacs/pmdm")')
    'https://hg.serna.eu/emacs/pmdm'
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        with open(os.path.join(tmpdir, name), 'w') as recipe_file:
            recipe_file.write(recipe)
        with open(os.path.join(tmpdir, 'script.el'), 'w') as script:
            script.write(
                requests.get(
                    'https://raw.githubusercontent.com/melpa/melpa/master/'
                    'package-build/package-recipe.el'
                ).text
                + f"""(let ((package-build-recipes-dir "{tmpdir}"))
                       (send-string-to-terminal
                        (package-recipe--upstream-url
                          (package-recipe-lookup "{name}"))))"""
            )
        return subprocess.check_output(['emacs', '--script', script.name]).decode()


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
    elif 'RECIPE' in os.environ:
        check_remote_package(os.environ['RECIPE'])
        sys.exit(return_code())
    else:
        check_melpa_pr_loop()
