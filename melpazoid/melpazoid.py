# -*- coding: utf-8 -*-
"""
Python module for checking MELPA recipes.

Test this file:
  pytest --doctest-modules

Use this file a script:
  python <this_file>.py
"""
from __future__ import print_function
import functools
import glob
import io  # noqa: F401 -- used by doctests
import os
import random
import re
import requests
import subprocess
import sys
import tempfile
import time
from typing import Iterator, TextIO, Tuple

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
    if not validate_recipe(recipe):
        _fail(f"Recipe '{recipe}' appears to be invalid")
        return
    files: list = _files_in_recipe(recipe, elisp_dir)
    subprocess.check_output(['rm', '-rf', '_elisp'])
    os.makedirs('_elisp')
    for ii, file in enumerate(files):
        target = os.path.basename(file) if file.endswith('.el') else file
        target = os.path.join('_elisp', target)
        os.makedirs(os.path.join('_elisp', os.path.dirname(file)), exist_ok=True)
        subprocess.check_output(['mv', os.path.join(elisp_dir, file), target])
        files[ii] = target
    _write_requirements(files, recipe)
    check_containerized_build(_package_name(recipe))
    check_license(files, elisp_dir, clone_address)
    print_related_packages(recipe)  # could throw ConnectionError
    print_packaging(recipe, files, pr_data, clone_address)


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


def validate_recipe(recipe: str) -> bool:
    """
    >>> validate_recipe('(abc :repo "xyz" :fetcher github)')
    True
    >>> validate_recipe('??')
    False
    """
    tokenized_recipe = _tokenize_expression(recipe)
    valid = (
        tokenized_recipe[0] == '('
        and tokenized_recipe[-1] == ')'
        and len([pp for pp in tokenized_recipe if pp == '('])
        == len([pp for pp in tokenized_recipe if pp == ')'])
    )
    return valid


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


def check_containerized_build(package_name: str):
    print(f"Building container for {package_name}... 🐳")
    output = subprocess.check_output(['make', 'test', f"PACKAGE_NAME={package_name}"])
    for line in output.decode().strip().split('\n'):
        # byte-compile-file writes ":Error: ", package-lint ": error: "
        if ':Error: ' in line or ': error: ' in line:
            _fail(line, highlight=r' ?[Ee]rror:')
        elif ':Warning: ' in line or ': warning: ' in line:
            _note(line, CLR_WARN, highlight=r' ?[Ww]arning:')
        elif line.startswith('### '):
            _note(line, CLR_INFO)
        elif not line.startswith('make[1]: Leaving directory'):
            print(line)


def _files_in_recipe(recipe: str, elisp_dir: str) -> list:
    files = _run_elisp_script(
        f"""
        (require 'package-build)
        (send-string-to-terminal
          (let* ((package-build-working-dir "{os.path.dirname(elisp_dir)}")
                 (rcp {_recipe_struct_elisp(recipe)}))
            (mapconcat (lambda (x) (format "%s" x))
                       (package-build--expand-source-file-list rcp) "\n")))
        """
    ).split('\n')
    return [file for file in files if os.path.exists(os.path.join(elisp_dir, file))]


@functools.lru_cache()
def _tokenize_expression(expression: str) -> list:
    """
    >>> _tokenize_expression('(shx :repo "riscy/xyz" :fetcher github) ; comment')
    ['(', 'shx', ':repo', '"riscy/xyz"', ':fetcher', 'github', ')']
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        with open(os.path.join(tmpdir, 'scratch'), 'w') as scratch:
            scratch.write(expression)
        parsed_expression = _run_elisp_script(
            f"""
            (send-string-to-terminal
              (format "%S" (with-temp-buffer (insert-file-contents "{scratch.name}")
                                             (read (current-buffer)))))
            """
        )
    parsed_expression = parsed_expression.replace('(', ' ( ')
    parsed_expression = parsed_expression.replace(')', ' ) ')
    tokenized_expression: list = parsed_expression.split()
    return tokenized_expression


def _package_name(recipe: str) -> str:
    """
    >>> _package_name('(shx :files ...)')
    'shx'
    """
    return _tokenize_expression(recipe)[1] if recipe else ''


def _main_file(recipe_files: list, recipe: str) -> str:
    """
    >>> _main_file(['_elisp/a.el', '_elisp/b.el'], '(a :files ...)')
    '_elisp/a.el'
    >>> _main_file(['a.el', 'b.el'], '(b :files ...)')
    'b.el'
    >>> _main_file(['a.el', 'a-pkg.el'], '(a :files ...)')
    'a-pkg.el'
    """
    package_name = _package_name(recipe)
    try:
        return next(
            el
            for el in sorted(recipe_files)
            if os.path.basename(el) == f"{package_name}-pkg.el"
            or os.path.basename(el) == f"{package_name}.el"
        )
    except StopIteration:
        return ''


def _write_requirements(recipe_files: list, recipe: str):
    """Create a little elisp script that Docker will run as setup."""
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
        elif filename.endswith('-pkg.el'):
            with open(filename, 'r') as pkg_el:
                reqs.append(_reqs_from_pkg_el(pkg_el))
        elif filename.endswith('.el'):
            with open(filename, 'r') as el_file:
                reqs.append(_reqs_from_el_file(el_file))
    reqs = sum([req.split('(')[1:] for req in reqs], [])
    reqs = [req.replace(')', '').strip().lower() for req in reqs if req.strip()]
    if with_versions:
        return set(reqs)
    return {req.split('"')[0].strip() for req in reqs}


def _reqs_from_pkg_el(pkg_el: TextIO) -> str:
    """
    >>> _reqs_from_pkg_el(io.StringIO('''(define-package "x" "1.2" "A pkg." '((emacs "31.5") (xyz "123.4")))'''))
    '( ( emacs "31.5" ) ( xyz "123.4" ) )'
    """
    reqs = pkg_el.read()
    reqs = ' '.join(_tokenize_expression(reqs))
    reqs = reqs[reqs.find('( (') :]
    reqs = reqs[: reqs.find(') )') + 3]
    return reqs


def _reqs_from_el_file(el_file: TextIO) -> str:
    """
    >>> _reqs_from_el_file(io.StringIO(';; x y z\\n ;; package-requires: ((emacs "24.4"))'))
    ';; package-requires: ((emacs "24.4"))'
    """
    for line in el_file.readlines():
        if re.match('[; ]*Package-Requires:', line, re.I):
            return line.strip()
    return ''


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
    """
    >>> _check_license_github_api('https://github.com/magit/magit.git')
    - GitHub API found `GNU General Public License v3.0`
    True
    """
    # TODO: gitlab also has a license API -- support it?
    # e.g. https://gitlab.com/api/v4/users/jagrg/projects ?
    if clone_address.endswith('.git'):
        clone_address = clone_address[:-4]
    match = re.search(r'github.com/([^"]*)', clone_address, flags=re.I)
    if not match:
        return False
    repo_suffix = match.groups()[0].rstrip('/')
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
    _fail('- Please add a LICENSE or COPYING file to the repository')
    return False


def _check_license_in_files(elisp_files: list) -> bool:
    """Check the elisp files themselves."""
    individual_files_licensed = True
    for elisp_file in elisp_files:
        if not elisp_file.endswith('.el'):
            continue
        license_ = _check_license_in_file(elisp_file)
        basename = os.path.basename(elisp_file)
        if not license_:
            _fail(f"- {basename} has no detectable license boilerplate")
            individual_files_licensed = False
    return individual_files_licensed


def _check_license_in_file(elisp_file: str) -> str:
    """Scan the elisp file for some recognized license text."""
    # TODO: this function could be more comprehensive; don't use grep
    licenses = [
        ('GPL', r'GNU.* General Public License'),
        ('ISC', r'Permission to use, copy, modify, and/or'),
        ('MIT', r'Permission is hereby granted, free of charge, to any person'),
        ('MIT', r'SPDX-License-Identifier: MIT'),
        ('Unlicense', 'This is free and unencumbered software released into',),
    ]
    for license_key, license_txt in licenses:
        try:
            subprocess.check_output(['grep', '-i', license_txt, elisp_file])
            return license_key
        except subprocess.CalledProcessError:
            pass
    return ''


def print_packaging(
    recipe: str, recipe_files: list, pr_data: dict = None, clone_address: str = None
):
    _note('\n### Package details ###\n', CLR_INFO)
    _print_recipe(recipe_files, recipe)
    _print_requirements(recipe_files, recipe)
    if pr_data and clone_address:
        print(f"- PR by {pr_data['user']['login']}: {clone_address}")
        if pr_data['user']['login'].lower() not in clone_address.lower():
            _note("  - NOTE: Repo and recipe owner don't match", CLR_WARN)
    _print_package_files(recipe_files)


def _print_recipe(recipe_files: list, recipe: str):
    print(f"```elisp\n{recipe}\n```")
    if ':files' in recipe and ':defaults' not in recipe:
        _note('- Prefer the default recipe, especially for small packages', CLR_WARN)
    if ':branch' in recipe:
        _note('- Only specify :branch in unusual cases', CLR_WARN)
    if 'gitlab' in recipe and (':repo' not in recipe or ':url' in recipe):
        _fail('- With the GitLab fetcher you MUST set :repo and you MUST NOT set :url')
    if not _main_file(recipe_files, recipe):
        _fail(f"- No .el file matches the name '{_package_name(recipe)}'!")


def _print_requirements(recipe_files: list, recipe: str):
    """Print the list of Package-Requires from the 'main' file.
    Report on any mismatches between this file and other files, since the ones
    in the other files will be ignored.
    """
    print('- Package-Requires: ', end='')
    main_requirements = _requirements(recipe_files, recipe, with_versions=True)
    print(', '.join(req for req in main_requirements) if main_requirements else 'n/a')
    for file in recipe_files:
        file_requirements = set(_requirements([file], with_versions=True))
        if file_requirements and file_requirements != main_requirements:
            _fail(
                f"  - Package-Requires mismatch between {os.path.basename(file)} and "
                f"{os.path.basename(_main_file(recipe_files, recipe))}!"
            )


def _print_package_files(recipe_files: list):
    for recipe_file in recipe_files:
        if os.path.isdir(recipe_file):
            print(f"- {CLR_ULINE}{recipe_file}{CLR_OFF} -- directory")
            continue
        with open(recipe_file) as stream:
            try:
                header = stream.readline()
                header = header.split('-*-')[0]
                header = header.split(' --- ')[1]
                header = header.strip()
            except (IndexError, UnicodeDecodeError):
                header = '(no elisp header)'
        print(
            f"- {CLR_ULINE}{recipe_file}{CLR_OFF}"
            f" ({_check_license_in_file(recipe_file) or 'unknown license'})"
            + (f" -- {header}" if header else "")
        )
        if recipe_file.endswith('-pkg.el'):
            _note(f"  - Consider excluding this file; MELPA will create one", CLR_WARN)


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


def check_recipe(recipe: str = ''):
    """Check a remotely-hosted package."""
    return_code(0)
    scm = _source_code_manager(recipe)
    with tempfile.TemporaryDirectory() as elisp_dir:
        # package-build prefers the directory to be named after the package:
        elisp_dir = os.path.join(elisp_dir, _package_name(recipe))
        clone_address = _clone_address(recipe)
        if _clone(clone_address, _branch(recipe), into=elisp_dir, scm=scm):
            run_checks(recipe, elisp_dir, clone_address)


def _clone(repo: str, branch: str, into: str, scm: str = 'git') -> bool:
    """Try to clone the repository; return whether we succeeded."""
    if not requests.get(repo).ok:
        _fail(f"Unable to locate '{repo}'")
        return False
    subprocess.check_output(['mkdir', '-p', into])
    if branch:
        git_command = [scm, 'clone', '-b', branch, repo, into]
    elif scm == 'git':
        # If a package's repository doesn't use the master branch, then the
        # Melpa recipe must specify the branch using the :branch keyword
        # https://github.com/melpa/melpa/pull/6712
        git_command = [scm, 'clone', '-b', 'master', repo, into]
    elif scm == 'hg':
        git_command = [scm, 'clone', repo, into]
    else:
        _fail(f"Unrecognized SCM: {scm}")
        return False
    # git clone prints to stderr, oddly enough:
    result = subprocess.run(git_command, stderr=subprocess.STDOUT)
    if result.returncode != 0:
        _fail('Unable to clone this (prefer "master" as the default branch)')
        return False
    return True


def _source_code_manager(recipe: str) -> str:
    """
    >>>
    _source_code_manager('(kanban :fetcher hg :url ...)')
    'hg'
    _source_code_manager('(kanban :fetcher gitlab :url ...)')
    'git'
    """
    tokenized_recipe = _tokenize_expression(recipe)
    if tokenized_recipe[tokenized_recipe.index(':fetcher') + 1] == 'hg':
        return 'hg'
    return 'git'


def _branch(recipe: str) -> str:
    """
    >>> _branch('(shx :branch "develop" ...)')
    'develop'
    >>> _branch('(shx ...)')
    ''
    """
    tokenized_recipe = _tokenize_expression(recipe)
    if ':branch' not in tokenized_recipe:
        return ''
    return tokenized_recipe[tokenized_recipe.index(':branch') + 1].strip('"')


def check_local_package(elisp_dir: str = None, package_name: str = None):
    """Check a locally-hosted package (WIP)."""
    return_code(0)
    elisp_dir = elisp_dir or input('Path: ').strip()
    assert os.path.isdir(elisp_dir)
    package_name = package_name or input(f"Name of package at {elisp_dir}: ")
    recipe = f'({package_name or "NONAME"} :repo "N/A")'
    run_checks(recipe, elisp_dir)


def check_melpa_pr(pr_url: str):
    """Check a PR on MELPA."""
    return_code(0)
    match = re.search(MELPA_PR, pr_url)  # MELPA_PR's 0th group has the number
    assert match

    pr_data = requests.get(f"{MELPA_PULL_API}/{match.groups()[0]}").json()
    if 'changed_files' not in pr_data:
        _note(f"{pr_url} does not appear to be a MELPA PR", CLR_ERROR)
        return
    if int(pr_data['changed_files']) != 1:
        _note('Please only add one recipe per pull request', CLR_ERROR)
        return
    filename, recipe = _filename_and_recipe(pr_data['diff_url'])
    if not filename or not recipe:
        _note(f"Unable to build the pull request at {pr_url}", CLR_ERROR)
        return

    clone_address: str = _clone_address(recipe)
    with tempfile.TemporaryDirectory() as elisp_dir:
        # package-build prefers the directory to be named after the package:
        elisp_dir = os.path.join(elisp_dir, _package_name(recipe))
        if _clone(clone_address, _branch(recipe), into=elisp_dir):
            run_checks(recipe, elisp_dir, clone_address, pr_data)


@functools.lru_cache()
def _filename_and_recipe(pr_data_diff_url: str) -> Tuple[str, str]:
    """Determine the filename and the contents of the user's recipe."""
    # TODO: use https://developer.github.com/v3/repos/contents/ instead of 'patch'
    diff_text = requests.get(pr_data_diff_url).text
    if (
        'new file mode' not in diff_text
        or 'a/recipes' not in diff_text
        or 'b/recipes' not in diff_text
    ):
        _note('This does not appear to add a new recipe', CLR_WARN)
        return '', ''
    with tempfile.TemporaryDirectory() as tmpdir:
        with subprocess.Popen(
            ['patch', '-s', '-o', os.path.join(tmpdir, 'patch')], stdin=subprocess.PIPE,
        ) as process:
            process.stdin.write(diff_text.encode())
        with open(os.path.join(tmpdir, 'patch')) as patch_file:
            basename = diff_text.split('\n')[0].split('/')[-1]
            return basename, patch_file.read().strip()


def _clone_address(recipe: str) -> str:
    """
    This is a HACK to get the clone address from the
    filename/recipe pair using the builtin MELPA machinery.  As a
    bonus, it validates the recipe.
    >>> _clone_address('(shx :repo "riscy/shx-for-emacs" :fetcher github)')
    'https://github.com/riscy/shx-for-emacs.git'
    >>> _clone_address('(pmdm :fetcher hg :url "https://hg.serna.eu/emacs/pmdm")')
    'https://hg.serna.eu/emacs/pmdm'
    """
    return _run_elisp_script(
        f"""
        (require 'package-recipe)
        (send-string-to-terminal
          (package-recipe--upstream-url {_recipe_struct_elisp(recipe)}))
        """
    )


@functools.lru_cache()
def _recipe_struct_elisp(recipe: str) -> str:
    """Turn the recipe into an elisp 'recipe' object."""
    name = _package_name(recipe)
    with tempfile.TemporaryDirectory() as tmpdir:
        with open(os.path.join(tmpdir, name), 'w') as recipe_file:
            recipe_file.write(recipe)
        return _run_elisp_script(
            f"""
            (require 'package-recipe)
            (let ((package-build-recipes-dir "{tmpdir}"))
              (send-string-to-terminal (format "%S" (package-recipe-lookup "{name}"))))
            """
        )


def _run_elisp_script(script: str) -> str:
    stderr = subprocess.STDERR if DEBUG else subprocess.DEVNULL
    with tempfile.TemporaryDirectory() as tmpdir:
        for filename, content in _package_build_files().items():
            with open(os.path.join(tmpdir, filename), 'w') as file:
                file.write(content)
        script = f"""(progn (add-to-list 'load-path "{tmpdir}") {script})"""
        result = subprocess.check_output(
            ['emacs', '--batch', '--eval', script], stderr=stderr
        )
        return result.decode().strip()


@functools.lru_cache()
def _package_build_files() -> dict:
    """Grab the required package-build files from the MELPA repo."""
    return {
        filename: requests.get(
            'https://raw.githubusercontent.com/melpa/melpa/master/'
            f'package-build/{filename}'
        ).text
        for filename in [
            'package-build-badges.el',
            'package-build.el',
            'package-recipe-mode.el',
            'package-recipe.el',
        ]
    }


@functools.lru_cache()
def _package_recipe_el() -> str:
    """Grab the source code for MELPA's package-build/package-recipe.el"""
    return requests.get(
        'https://raw.githubusercontent.com/melpa/melpa/master/'
        'package-build/package-recipe.el'
    ).text


def check_melpa_pr_loop():
    """Check MELPA pull requests in a loop."""
    for pr_url in _fetch_pull_requests():
        print(f"Found MELPA PR {pr_url}")
        check_melpa_pr(pr_url)
        if return_code() != 0:
            _fail('*** This PR failed')
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
        check_recipe(os.environ['RECIPE'])
        sys.exit(return_code())
    elif 'RECIPE_FILE' in os.environ:
        with open(os.environ['RECIPE_FILE'], 'r') as recipe_file:
            check_recipe(recipe_file.read())
        sys.exit(return_code())
    else:
        check_melpa_pr_loop()
