"""
usage: melpazoid.py [-h] [--license] [--recipe RECIPE] [target]

positional arguments:
  target           a recipe, a path to a recipe or package, or a MELPA PR URL

optional arguments:
  -h, --help       show this help message and exit
  --license        only check licenses
  --recipe RECIPE  a valid MELPA recipe
"""

from __future__ import annotations

__author__ = 'Chris Rayner <dchrisrayner@gmail.com>'
__license__ = 'SPDX-License-Identifier: GPL-3.0-or-later'
import argparse
import configparser
import functools
import json
import operator
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.request
from pathlib import Path
from typing import TYPE_CHECKING, Any, TextIO

if TYPE_CHECKING:
    from collections.abc import Iterator

_RETURN_CODE = 0  # eventual return code when run as script
_MELPAZOID_ROOT = (Path(__file__).parent if '__file__' in vars() else Path.cwd()).parent

# define the colors of the report (or none), per https://no-color.org
# https://misc.flogisoft.com/bash/tip_colors_and_formatting
NO_COLOR = os.environ.get('NO_COLOR', False)
CLR_OFF = '' if NO_COLOR else '\033[0m'
CLR_ERROR = '' if NO_COLOR else '\033[31m'
CLR_WARN = '' if NO_COLOR else '\033[33m'
CLR_INFO = '' if NO_COLOR else '\033[32m'

MELPA_PR = r'https?://github.com/melpa/melpa/pull/([0-9]+)'


def _return_code(return_code: int | None = None) -> int:
    """Return (and optionally set) the current return code.
    If return_code matches env var EXPECT_ERROR, return 0 --
    this is useful for running CI checks on melpazoid itself.
    """
    global _RETURN_CODE  # noqa: PLW0603
    if return_code is not None:
        _RETURN_CODE = return_code
    expect_error = int(os.environ.get('EXPECT_ERROR', 0))
    return 0 if expect_error == _RETURN_CODE else _RETURN_CODE


def is_recipe(recipe: str) -> bool:
    """Validate whether the recipe looks correct.
    >>> assert is_recipe('(abc :repo "xyz" :fetcher github) ; abc recipe!')
    >>> assert not is_recipe('(a/b :repo "xyz" :fetcher github)')
    >>> assert not is_recipe('??')
    """
    try:
        tokens = _tokenize_expression(recipe)
    except ValueError:
        return False
    len_minimal_recipe = 7  # 1 for name, 2 for repo, 2 for fetcher, 2 for parens
    if len(tokens) < len_minimal_recipe or tokens[0] != '(' or tokens[-1] != ')':
        return False
    if not re.match(r"[A-Za-z\-]", tokens[1]):
        return False
    try:
        return bool(_recipe_struct_elisp(recipe))
    except ChildProcessError:
        return False


def _note(message: str, color: str = '', highlight: str = '') -> None:
    """Print a note, possibly in color, possibly highlighting specific text."""
    if highlight:
        print(re.sub(f"({highlight})", f"{color}\\g<1>{CLR_OFF}", message))
    else:
        print(f"{color}{message}{CLR_OFF}")


def _fail(message: str, color: str = CLR_ERROR, highlight: str = '') -> None:
    _note(message, color, highlight)
    _return_code(2)


def check_containerized_build(recipe: str, elisp_dir: Path) -> None:
    """Build a Docker container to run checks on elisp_dir, given a recipe."""
    if not is_recipe(recipe):
        _fail(f"Not a valid recipe: {recipe}")
        return

    files = [f.relative_to(elisp_dir) for f in _files_in_recipe(recipe, elisp_dir)]
    elisp_files = [file_.name for file_ in files if file_.name.endswith('.el')]
    if len(elisp_files) != len(set(elisp_files)):
        _fail(f"Multiple .el files with the same name: {' '.join(sorted(elisp_files))}")
        return

    pkg_dir = _MELPAZOID_ROOT / 'pkg'
    shutil.rmtree(pkg_dir, ignore_errors=True)
    for ii, file in enumerate(files):
        files[ii] = pkg_dir / (file.name if file.name.endswith('.el') else file)
        files[ii].parent.mkdir(parents=True, exist_ok=True)
        # shutil.copy/copytree won't work here because file can be a file or a dir:
        subprocess.run(['cp', '-r', str(elisp_dir / file), files[ii]], check=True)
    _write_requirements(files)

    _note(f"<!-- Building container for {package_name(recipe)}... 🐳 -->")
    run_env = dict(os.environ, DOCKER_OUTPUT='--quiet')  # or --progress=plain
    cmd = ['make', '-C', str(_MELPAZOID_ROOT), 'image']
    subprocess.run(cmd, check=True, env=run_env)

    _note('\n<!-- Running tests... -->')
    cmd = ['make', '-C', str(_MELPAZOID_ROOT), 'test']
    main_file = _main_file(files, recipe)
    if main_file and sum(f.name.endswith('.el') for f in pkg_dir.iterdir()) > 1:
        cmd.append(f"PACKAGE_MAIN={main_file.name}")
    run_result = subprocess.run(cmd, capture_output=True, check=False, env=run_env)
    lines = run_result.stdout.decode().strip().split('\n')
    if run_result.stderr:
        lines += ['\nStderr output while compiling/loading:']
        lines += ['```', run_result.stderr.decode().strip(), '```']
    for line in lines:
        # byte-compile-file writes ":Error: ", package-lint ": error: "
        if ':Error: ' in line or ': error: ' in line:
            _fail(line, highlight=r' ?[Ee]rror:')
        elif ':Warning: ' in line or ': warning: ' in line:
            _note(line, CLR_WARN, highlight=r' ?[Ww]arning:')
        elif line.startswith('### '):
            _note(line, CLR_INFO)
        elif not line.startswith('make[1]: Leaving directory'):
            print(line)
    print_packaging(recipe, elisp_dir)


def _files_in_recipe(recipe: str, elisp_dir: Path) -> list[Path]:
    """Return a file listing, relative to elisp_dir.
    Raise ChildProcessError if the recipe does not work against elisp_dir.
    >>> _files_in_recipe('(melpazoid :fetcher github :repo "xyz")', Path('melpazoid'))
    [PosixPath('melpazoid/melpazoid.el')]
    """
    filenames = eval_elisp(
        f"""
        (require 'package-build)
        (setq package-build-working-dir "{elisp_dir.parent}")
        (setq rcp {_recipe_struct_elisp(recipe)})
        (send-string-to-terminal
            (mapconcat #'car (package-build-expand-files-spec rcp t) "\n"))
        """
    ).split('\n')
    files = [elisp_dir / filename for filename in filenames]
    return sorted(file for file in files if file.exists())


def _default_recipe(recipe: str) -> str:
    """Simplify the given recipe by removing ':files'.
    >>> _default_recipe('(recipe :fetcher hg :repo "a/b" :branch na :files ("*.el"))')
    '(recipe :fetcher hg :repo "a/b" :branch na)'
    >>> _default_recipe('(recipe :fetcher hg :url "a/b")')
    '(recipe :fetcher hg :url "a/b")'
    """
    tokens = _tokenize_expression(recipe)
    fetcher = tokens.index(':fetcher')
    repo = tokens.index(':repo' if ':repo' in tokens else ':url')
    indices = [1, fetcher, fetcher + 1, repo, repo + 1]
    if ':branch' in tokens:
        branch = tokens.index(':branch')
        indices += [branch, branch + 1]
    return '(' + ' '.join(operator.itemgetter(*indices)(tokens)) + ')'


def _tokenize_expression(expression: str) -> list[str]:
    """Turn an elisp expression into a list of tokens.
    Raise ValueError if the expression can't be parsed.
    >>> _tokenize_expression('(shx :repo "riscy/xyz" :fetcher github) ; comment')
    ['(', 'shx', ':repo', '"riscy/xyz"', ':fetcher', 'github', ')']
    """
    lexer = shlex.shlex(expression)
    lexer.quotes = '"'
    lexer.commenters = ';'
    lexer.wordchars = lexer.wordchars + "':-"
    tokens = list(lexer)
    unbalanced_parens = 0
    for t in tokens:
        if t == '(':
            unbalanced_parens += 1
        if t == ')':
            unbalanced_parens -= 1
            if unbalanced_parens < 0:
                break
    if unbalanced_parens == 0:
        return tokens
    raise ValueError(f"Unbalanced expression: {expression}")


def package_name(recipe: str) -> str:
    """Return the package's name, based on the recipe.
    >>> package_name('(shx :files ...)')
    'shx'
    """
    return _tokenize_expression(recipe)[1]


def _main_file(files: list[Path], recipe: str) -> Path | None:
    """Figure out the 'main' file of the recipe, per MELPA convention.
    >>> _main_file([Path('pkg/a.el'), Path('pkg/b.el')], '(a :files ...)')
    PosixPath('pkg/a.el')
    >>> _main_file([Path('a.el'), Path('b.el')], '(b :files ...)')
    PosixPath('b.el')
    >>> _main_file([Path('a.el'), Path('a-pkg.el')], '(a :files ...)')
    PosixPath('a-pkg.el')
    """
    name = package_name(recipe)
    try:
        return next(
            el
            # the code in #'package-build--build-multi-file-package that
            # determines the 'main' file is not quite exposed but it first
            # looks for name-pkg.el, then name-pkg.el.in, and then name.el,
            # which happens to match sorted() order:
            for el in sorted(files)
            if el.name in (f"{name}-pkg.el", f"{name}-pkg.el.in", f"{name}.el")
        )
    except StopIteration:
        return None


def _write_requirements(files: list[Path]) -> None:
    """Create a little elisp script that Docker will run as setup."""
    with Path('_requirements.el').open('w', encoding='utf-8') as requirements_el:
        requirements_el.write(
            f";; {time.strftime('%Y-%m-%d')} ; helps to invalidate old Docker cache\n\n"
            + ";; NOTE: emacs --script <file.el> will set `load-file-name' to <file.el>\n"
            + ";; which can disrupt the compilation of packages that use that variable:\n"
            + "(setq load-file-name nil)\n"
            + ";; (setq network-security-level 'low)  ; expired certs last resort\n"
            + "(require 'package)\n"
            + "(package-initialize)\n"
            + """(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))\n"""
            + "(package-refresh-contents)\n"
            + "(setq package-install-upgrade-built-in t)\n"
            + "(package-install 'pkg-info)\n"
            + "(package-install 'package-lint)\n"
        )
        for req in requirements(files):
            req_, *version_maybe = req.split()
            version = version_maybe[0].strip('"') if version_maybe else 'N/A'
            if req_ == 'emacs':
                continue
            if '"' in req_:  # common failure mode: misplaced quotation marks
                _fail(f"- Package names should not be quoted: `{req}`")
                continue
            if req_ == 'marginalia':
                _fail(
                    "- Don't require marginalia: https://github.com/minad/marginalia#adding-custom-annotators-or-classifiers"
                )
            # always install the latest available version of the dependency.
            requirements_el.write(
                f'\n(message "Installing {req_} {version} or later")\n'
                + f"(ignore-errors (package-install (cadr (assq '{req_} package-archive-contents))))\n"
            )


def requirements(files: list[Path]) -> set[str]:
    """Return (downcased) requirements given a listing of files.
    If a recipe is given, use it to determine which file is the main file;
    otherwise scan every .el file for requirements.
    """
    reqs: set[str] = set()
    for file_ in (f for f in files if f.is_file()):
        with file_.open(encoding='utf-8', errors='replace') as stream:
            try:
                if file_.name.endswith('-pkg.el'):
                    reqs = reqs.union(_reqs_from_pkg_el(stream))
                elif file_.name.endswith('.el'):
                    reqs = reqs.union(_reqs_from_el_file(stream))
            except ValueError as err:
                _fail(f"Couldn't parse requirements in {file_.name}: {err}")
    return reqs


def _reqs_from_pkg_el(pkg_el: TextIO) -> set[str]:
    """Pull the requirements out of a -pkg.el file.
    >>> import io
    >>> sorted(_reqs_from_pkg_el(io.StringIO(
    ...   '''(define-package "x" "1.2" "A pkg." '(a (b "31.5")))''')))
    ['a', 'b "31.5"']
    """
    # TODO: fails if EXTRA-PROPERTIES args were given to #'define-package
    reqs = pkg_el.read()
    reqs = ' '.join(_tokenize_expression(reqs)[5:-1])
    reqs = reqs[reqs.find("' (") + 2 :]
    reqs = reqs[: reqs.find(') )') + 3]
    return {
        req.replace(')', '').strip().lower()
        for req in re.split('[()]', reqs)
        if req.strip()
    }


def _reqs_from_el_file(el_file: TextIO) -> set[str]:
    """Hacky function to pull the requirements out of an elisp file.
    >>> import io
    >>> _reqs_from_el_file(io.StringIO(';; package-requires: ((emacs "24.4"))'))
    {'emacs "24.4"'}
    """
    # TODO: if Package-Requires crosses multiple lines, parsing will fail.
    # This is also currently an issue with package-lint (2024/09/02)
    for line in el_file:
        match = re.match(r'[; ]*Package-Requires[ ]*:[ ]*(.*)$', line, re.I)
        if match:
            _tokenize_expression(match.groups()[0])
            return {
                req.replace(')', '').strip().lower()
                for req in re.split('[()]', match.groups()[0])
                if req.strip()
            }
    return set()


def _check_license_api(clone_address: str) -> bool:
    """Use the GitHub or GitLab API to check for a license.
    Return False if unable to check (e.g. it's not on GitHub).
    >>> _check_license_api('https://github.com/riscy/elfmt')
    True
    """
    repo_info = _repo_info_api(clone_address)
    if repo_info is None:
        return False

    license_ = repo_info.get('license')
    if not license_:
        _fail('- Add a LICENSE file to the repository')
        print('  See: https://github.com/licensee/licensee')
        return True

    gpl_compatible_licensee_licenses = {
        'Apache License 2.0',
        'BSD 2-Clause "Simplified" License',
        'BSD 3-Clause "New" or "Revised" License',
        'BSD Zero Clause License',  # https://github.com/melpa/melpa/pull/7189
        'Creative Commons Zero v1.0 Universal',
        'Do What The F*ck You Want To Public License',
        'GNU Affero General Public License v3.0',
        # re: GPL v2.0 see https://github.com/johannes-mueller/company-wordfreq.el/issues/6
        'GNU General Public License v2.0 or later',
        'GNU General Public License v3.0 only',
        'GNU General Public License v3.0 or later',
        'GNU General Public License v3.0',
        'GNU Lesser General Public License v3.0',
        'ISC License',
        'MIT License',
        'Mozilla Public License 2.0',
        'The Unlicense',
    }

    if license_.get('name') in gpl_compatible_licensee_licenses:
        pass
    elif license_.get('name') == 'Other':
        _note('- Try to use a standard license file format for your repo', CLR_WARN)
        print('  This helps detection tools like: https://github.com/licensee/licensee')
    else:
        _note(f"- License {license_.get('name')} may not be compatible", CLR_WARN)
    return True


@functools.lru_cache
def _repo_info_api(clone_address: str) -> dict[str, Any] | None:
    """Use the GitHub or GitLab API to fetch details about a repository.
    Raise urllib.error.URLError if API request fails.
    """
    repo_info: dict[str, Any]
    if clone_address.endswith('.git'):
        clone_address = clone_address[:-4]
    match = re.search(r'github.com/([^"]*)', clone_address, flags=re.I)
    if match:
        project_id = match.groups()[0].rstrip('/')
        repo_info = json.loads(_url_get(f"https://api.github.com/repos/{project_id}"))
        return repo_info

    match = re.search(r'gitlab.com/([^"]*)', clone_address, flags=re.I)
    if match:
        project_id = match.groups()[0].rstrip('/').replace('/', '%2F')
        gitlab_projects = 'https://gitlab.com/api/v4/projects'
        repo_info = json.loads(_url_get(f"{gitlab_projects}/{project_id}?license=true"))
        # HACK: align GitLab API response with typical GitHub api response
        repo_info['updated_at'] = repo_info['last_activity_at']
        repo_info['watchers_count'] = repo_info['star_count']
        return repo_info

    return None


def _check_license_file(elisp_dir: Path) -> None:
    """Scan any COPYING or LICENSE files."""
    license_names = (
        'copying',
        'copying.txt',
        'license',
        'license.md',
        'license.txt',
        'licenses',
        'unlicense',
    )
    has_license_file = False
    for license_ in elisp_dir.iterdir():
        # handles e.g. LICENSE.GPL, LICENSE.APACHE
        if not any(license_.name.lower().startswith(name) for name in license_names):
            continue
        has_license_file = True
        if license_.is_dir():
            licenses = ', '.join(f"`{f.name}`" for f in license_.iterdir())
            print(f"- {license_.name} directory: {licenses}")
            return
        with license_.open(encoding='utf-8', errors='replace') as stream:
            excerpt = ' '.join(stream.read(200).split())[:50]
            print(f"- {license_.name} excerpt: `{excerpt}`...")
    if not has_license_file:
        _fail('- Add a GPL-compatible LICENSE file to the repository')


def _check_file_for_license_boilerplate(el_file: TextIO) -> str | None:
    """Check an elisp file for some license boilerplate.
    >>> import io
    >>> _check_file_for_license_boilerplate(io.StringIO('SPDX-License-Identifier: ISC'))
    'ISC License'
    >>> _check_file_for_license_boilerplate(
    ...   io.StringIO('This program is free software: you can redistribute it'))
    'GPL*'
    """
    text = el_file.read()
    match = re.search(r'SPDX-License-Identifier:[ ]*(.+)', text, flags=re.I)
    if match:
        # TODO: one can AND and OR licenses together
        # https://spdx.github.io/spdx-spec/v2.3/SPDX-license-expressions/
        license_id = match.groups()[0]
        license_ = _spdx_license(license_id)
        if license_ is None:
            _fail(f"- Invalid SPDX id `{license_id}`; check https://spdx.dev/ids/")
            return None
        if not license_['isFsfLibre']:
            _fail(f"- Non-free/libre license: {match.groups()[0]}")
        return str(license_['name'])

    gpl_compatible_license_excerpts = {
        # NOTE: consider using https://github.com/emacscollective/elx instead
        'Apache License 2.0': 'Licensed under the Apache License, Version 2.0',
        'BSD*': 'Redistribution and use in source and binary forms',
        'FSFAP': 'Copying and distribution of this file, with or without',
        'GPL*': 'is free software.* you can redistribute it',
        '0BSD/ISC License': 'Permission to use, copy, modify, and/or distribute this',
        'MIT License': 'Permission is hereby granted, free of charge, to any person',
        'MPL-2': 'This source code form is subject to the terms of the Mozilla',
        'The Unlicense': 'This is free and unencumbered software released into',
    }
    for license_key, license_text in gpl_compatible_license_excerpts.items():
        if re.search(license_text, text, re.IGNORECASE):
            return license_key
    return None


@functools.lru_cache
def _spdx_license(license_id: str) -> dict[str, Any] | None:
    for operator_ in (' OR ', ' AND ', ' WITH '):
        # the SPDX API does not handle SPDX expressions; take a stab at it here:
        if operator_ in license_id:
            _note(f"- Reviewer note: nontrivial SPDX license '{license_id}'", CLR_INFO)
            license_id = license_id.split(operator_)[0]
            break
    license_id = license_id.replace(' ', '-')
    try:
        response = _url_get(f'https://spdx.org/licenses/{license_id.strip()}.json')
        return dict(json.loads(response))
    except urllib.error.HTTPError:
        return None


def print_packaging(recipe: str, elisp_dir: Path) -> None:
    """Print additional details (how it's licensed, what files, etc.)"""
    print('\n⸺ Package and license:')
    _check_recipe(recipe, elisp_dir)
    _check_package_requires(recipe, elisp_dir)
    _check_url(recipe, elisp_dir)
    _check_license(recipe, elisp_dir)
    _check_other(recipe, elisp_dir)
    _check_package_tags(recipe)
    for file_ in (_MELPAZOID_ROOT / 'pkg').rglob('*'):
        relpath = file_.relative_to(_MELPAZOID_ROOT)
        if file_.is_dir():
            continue
        with file_.open(encoding='utf-8', errors='replace') as stream:
            boilerplate = _check_file_for_license_boilerplate(stream)
        print(f"- {relpath}: {boilerplate or 'license unknown'}")
    if repo_info := _repo_info_api(_clone_address(recipe)):
        print('- Repository:', (repo_info['license'] or {}).get('name', 'Unlicensed'))
        if repo_info.get('archived'):
            _fail('- GitHub repository is archived')


def _check_url(recipe: str, elisp_dir: Path) -> None:
    for file in _files_in_recipe(recipe, elisp_dir):
        if not file.name.endswith('.el') or file.name.endswith('-pkg.el'):
            continue
        with file.open(encoding='utf-8', errors='replace') as stream:
            text = stream.read()
        url_match = re.search(r';; URL:[ ]*(.+)', text, flags=re.I)
        if url_match:
            url = url_match.groups()[0]
            if not _url_ok(url):
                _fail(f"- Unreachable package URL in {file.name}: {url!r}")


def _check_package_tags(recipe: str) -> None:
    # Example of rationale: https://github.com/melpa/melpa/pull/9074
    clone_address = _clone_address(recipe)
    if clone_address.endswith('.git'):
        clone_address = clone_address[:-4]
    if match := re.search(r'github.com/([^"]*)', clone_address, flags=re.I):
        repo = match.groups()[0].rstrip('/')
        if tags := json.loads(_url_get(f"https://api.github.com/repos/{repo}/tags")):
            reminder = f"- In case you haven't, ensure GitHub release {tags[0]['name']} is up-to-date with your current code and `Package-Version`"
            _note(reminder, CLR_WARN)


def _check_other(recipe: str, elisp_dir: Path) -> None:
    files_in_recipe = _files_in_recipe(recipe, elisp_dir)
    if not any(file.name == f"{package_name(recipe)}.el" for file in files_in_recipe):
        _fail(f"- MELPA requires a file called {package_name(recipe)}.el")
    for file in files_in_recipe:
        if not file.name.endswith('.el'):
            continue
        relpath = file.relative_to(elisp_dir)
        if file.name == f"{package_name(recipe)}-pkg.el":
            _note(
                f"- {relpath} -- consider excluding; "
                + f"MELPA can create one from {package_name(recipe)}.el",
                CLR_WARN,
            )
            continue
        if file.name.endswith('-pkg.el'):
            _fail(f"- {relpath} -- files ending in `-pkg.el` are only for packaging")
            continue
        prefix = package_name(recipe)
        prefix = prefix[:-5] if prefix.endswith('-mode') else prefix
        if not re.match(f"^{prefix}[-.]", file.name):
            _fail(f"- {relpath} -- not in package namespace `{prefix}-`")
        with file.open(encoding='utf-8', errors='replace') as stream:
            try:
                header = stream.readline()
                header = header.split('-*-')[0]
                header = header.split(' --- ')[1]
                header = header.strip()
            except IndexError:
                _fail(f"- {relpath} -- no packaging header")


def _check_license(recipe: str, elisp_dir: Path) -> None:
    if not _check_license_api(_clone_address(recipe)):
        _check_license_file(elisp_dir)
    for file in _files_in_recipe(recipe, elisp_dir):
        if not file.name.endswith('.el'):
            continue
        relpath = file.relative_to(elisp_dir)
        with file.open(encoding='utf-8', errors='replace') as stream:
            if not _check_file_for_license_boilerplate(stream):
                _fail(
                    f"- {relpath} needs *formal* license boilerplate and/or an"
                    + " [SPDX-License-Identifier](https://spdx.dev/ids/)"
                )


def _check_recipe(recipe: str, elisp_dir: Path) -> None:
    files = _files_in_recipe(recipe, elisp_dir)
    for specifier in (':branch', ':commit', ':version-regexp'):
        if specifier in recipe:
            _note(f"- Avoid `{specifier}` in recipes except in unusual cases", CLR_WARN)
    if not _main_file(files, recipe):
        _fail(f"- No 'main' file found, e.g. '{package_name(recipe)}.el'")
    if ':url' in recipe and 'https://github.com' in recipe:
        _fail('- Use `:fetcher github :repo <repo>` instead of `:url`')
    if ':repo' in recipe and recipe.index(':fetcher') > recipe.index(':repo'):
        _note('- Please specify `:fetcher` before `:repo` in your recipe', CLR_WARN)
    if ':url' in recipe and recipe.index(':fetcher') > recipe.index(':url'):
        _note('- Please specify `:fetcher` before `:url` in your recipe', CLR_WARN)
    if ':files' in recipe:
        try:
            files_default_recipe = _files_in_recipe(_default_recipe(recipe), elisp_dir)
        except ChildProcessError:
            _note(f"<!-- Default recipe is unusable: {_default_recipe(recipe)} -->")
            files_default_recipe = []
        if files == files_default_recipe:
            _note(f"- Prefer equivalent recipe: `{_default_recipe(recipe)}`", CLR_WARN)
            return
        if '"*.el"' in recipe and ':defaults' not in recipe:
            new_recipe = recipe.replace('"*.el"', ':defaults')
            if files == _files_in_recipe(new_recipe, elisp_dir):
                _note(f"- Prefer equivalent recipe: `{new_recipe}`", CLR_WARN)
                return
            _note('- Prefer :defaults instead of *.el, if possible')
        if '"*.el"' in recipe:
            _note(f"- Prefer `{package_name(recipe)}*.el` over `*.el`", CLR_WARN)


def _check_package_requires(recipe: str, elisp_dir: Path) -> None:
    """Print the list of Package-Requires from the 'main' file.
    Report on any mismatches between this file and other files, since the ones
    in the other files will be ignored.
    """
    files = _files_in_recipe(recipe, elisp_dir)
    main_file = _main_file(files, recipe)
    if not main_file:
        _fail("- Can't check Package-Requires if there is no 'main' file")
        return
    main_file_requirements = requirements([main_file])
    for file in files:
        file_requirements = requirements([file])
        if file_requirements - main_file_requirements > set():
            _fail(
                f"- {main_file.name} must include all of the "
                + f"Package-Requires listed in {file.name}, including: "
                + ', '.join(sorted(file_requirements - main_file_requirements))
            )
    compat = next((r for r in main_file_requirements if r.startswith('compat ')), None)
    if compat:
        _note(f"- Reviewer note: this package depends on {compat}", CLR_INFO)


def check_package_name(name: str) -> None:
    """Print list of similar, or at least similarly named, packages.
    Report any occurrences of invalid/reserved package names.
    This function will print nothing if there are no issues.
    """
    # is the package name implicitly reserved?
    reserved_names = (
        # used by distel
        '^erl$',
        '^derl$',
        '^epmd$',
        '^erlext$',
        '^mcase$',
        '^net-fsm$',
        # other
        '^git-rebase$',
        '^helm-source-',
    )
    name_reserved = any(re.match(reserved, name) for reserved in reserved_names)
    if name_reserved:
        print('\n⸺ Package name:')
        _fail(f"- Error: `{name}` is reserved\n", highlight='Error')
        return

    # is the package name an Emacs builtin?
    try:
        eval_elisp(f"(require '{name})")
    except ChildProcessError:
        pass
    else:
        print('\n⸺ Package name:')
        _fail(f"- Error: `{name}` is an Emacs builtin\n", highlight='Error')
        return

    # do other packages have the same name (within some magin)?
    emacsmirror = emacsmirror_packages()
    same_names = [name, f"{name}-mode"]
    same_names += [name[:-5]] if name.endswith('-mode') else []
    same_names += [name[:-1]] if name.endswith('s') else []
    same_names += ['org-' + name[3:]] if name.startswith('ox-') else []
    same_names += ['ox-' + name[4:]] if name.startswith('org-') else []
    same_names = [name_ for name_ in same_names if name_ in emacsmirror]
    resolved_same = {name_: url for name_, url in emacsmirror.items() if name_ == name}
    resolved_same.update(emacsattic_packages(*same_names))
    resolved_same.update(emacswiki_packages(*same_names))
    resolved_same.update(elpa_packages(*same_names))
    resolved_same.update(melpa_packages(*same_names))
    for name_, url in resolved_same.items():
        print('\n⸺ Package name:')
        _fail(f"- `{name_}` may already exist: {url}\n")
        return

    # do other packages have similar names, especially namespace conflicts
    # (to save network calls, we only scan packages listed on emacsmirror):
    tokens = name.split('-')
    prefices = ['-'.join(tokens[: i + 1]) for i in range(len(tokens))]
    similar_names = {  # packages that are implicitly a parent of 'name'
        name_: url
        for name_, url in emacsmirror.items()
        if any(name_ == prefix for prefix in prefices)
    }
    similar_names.update(
        {  # packages that 'name' is implicitly a parent of
            name_: url
            for name_, url in emacsmirror.items()
            if name_.startswith(f"{name}-")
        }
    )
    if similar_names:
        similar_names.update(melpa_packages(*similar_names.keys()))
        print('\n⸺ Package name:')
        for name_, url in similar_names.items():
            print(f"- `{name_}` is similar: {url}")


@functools.lru_cache
def emacsattic_packages(*keywords: str) -> dict[str, str]:
    """(Obsolete) packages on Emacsattic matching 'keywords'.
    q.v. https://github.com/melpa/melpa/pull/8621#issuecomment-1616925395
    >>> emacsattic_packages('sos')
    {'sos': 'https://github.com/emacsattic/sos'}
    """
    packages = {kw: f"https://github.com/emacsattic/{kw}" for kw in keywords}
    return {kw: url for kw, url in packages.items() if _url_ok(url)}


@functools.lru_cache
def emacswiki_packages(*keywords: str) -> dict[str, str]:
    """Packages on emacswiki.org mirror matching 'keywords'.
    >>> emacswiki_packages('rss')
    {'rss': 'https://github.com/emacsmirror/emacswiki.org/blob/master/rss.el'}
    """
    packages = {}
    for keyword in set(keywords):
        el_file = keyword if keyword.endswith('.el') else keyword + '.el'
        pkg = f"https://github.com/emacsmirror/emacswiki.org/blob/master/{el_file}"
        if _url_ok(pkg):
            packages[keyword] = pkg
    return packages


@functools.lru_cache
def emacsmirror_packages() -> dict[str, str]:
    """All mirrored packages."""
    epkgs = 'https://raw.githubusercontent.com/emacsmirror/epkgs/master/.gitmodules'
    epkgs_parser = configparser.ConfigParser()
    epkgs_parser.read_string(_url_get(epkgs))
    return {
        epkg.split('"')[1]: data['url'] + ' (via emacsmirror)'
        for epkg, data in epkgs_parser.items()
        if epkg != 'DEFAULT'
    }


@functools.lru_cache
def elpa_packages(*keywords: str) -> dict[str, str]:
    """ELPA packages matching keywords.
    >>> elpa_packages('git-modes')
    {'git-modes (nongnu)': 'https://elpa.nongnu.org/nongnu/git-modes.html'}
    >>> sorted(elpa_packages('ivy'))
    ['ivy', 'ivy (devel)']
    """
    # q.v. http://elpa.gnu.org/packages/archive-contents
    elpa = 'https://elpa.gnu.org'
    nongnu_elpa = 'https://elpa.nongnu.org'
    sources = {
        **{f"{kw} (devel)": f"{elpa}/devel/{kw}.html" for kw in keywords},
        **{kw: f"{elpa}/packages/{kw}.html" for kw in keywords},
        **{f"{kw} (nongnu)": f"{nongnu_elpa}/nongnu/{kw}.html" for kw in keywords},
    }
    return {kw: url for kw, url in sources.items() if _url_ok(url)}


@functools.lru_cache
def melpa_packages(*keywords: str) -> dict[str, str]:
    """MELPA packages matching keywords.
    >>> melpa_packages('highlight-symbol')
    {'highlight-symbol': 'https://melpa.org/#/highlight-symbol'}
    """
    # q.v. 'http://melpa.org/archive.json'
    sources = {
        kw: f"https://github.com/melpa/melpa/blob/master/recipes/{kw}"
        for kw in keywords
    }
    return {
        kw: f"https://melpa.org/#/{kw}" for kw, url in sources.items() if _url_ok(url)
    }


def check_melpa_recipe(recipe: str) -> None:
    """Check a MELPA recipe definition."""
    _return_code(0)
    with tempfile.TemporaryDirectory() as tmpdir:
        # package-build prefers the directory to be named after the package:
        elisp_dir = Path(tmpdir) / package_name(recipe)
        clone_address = _clone_address(recipe)
        local_repo = _local_repo()
        if local_repo:
            print(f"Using local repository at {local_repo}")
            shutil.copytree(local_repo, elisp_dir)
            check_containerized_build(recipe, elisp_dir)
        elif _clone(clone_address, elisp_dir, _branch(recipe), _fetcher(recipe)):
            check_containerized_build(recipe, elisp_dir)


def check_license(recipe: str) -> None:
    """Check license for a given recipe."""
    # TODO: DRY up wrt check_melpa_recipe
    _return_code(0)
    with tempfile.TemporaryDirectory() as tmpdir:
        # package-build prefers the directory to be named after the package:
        elisp_dir = Path(tmpdir) / package_name(recipe)
        clone_address = _clone_address(recipe)
        local_repo = _local_repo()
        if local_repo:
            print(f"Using local repository at {local_repo}")
            shutil.copytree(local_repo, elisp_dir)
            _check_license(recipe, elisp_dir)
        elif _clone(clone_address, elisp_dir, _branch(recipe), _fetcher(recipe)):
            _check_license(recipe, elisp_dir)


def _fetcher(recipe: str) -> str:
    """Get the 'fetcher' property from a recipe.
    >>> _fetcher('(recipe :repo a/b :fetcher hg)')
    'hg'
    """
    tokenized_recipe = _tokenize_expression(recipe)
    return tokenized_recipe[tokenized_recipe.index(':fetcher') + 1]


def _local_repo() -> Path | None:
    if not os.environ.get('LOCAL_REPO'):
        return None
    return Path.expanduser(Path(os.environ['LOCAL_REPO']))


def _clone(repo: str, into: Path, branch: str | None, fetcher: str) -> bool:
    """Try to clone the repository; return whether we succeeded."""
    _note(f"<!-- Cloning {repo} {'@' + branch if branch else ''} -->")

    # check if we're being used in GitHub CI -- if so, modify the branch
    if not branch and 'RECIPE' in os.environ:
        branch = (
            os.environ.get('CI_BRANCH', '')
            or os.path.split(os.environ.get('GITHUB_REF', ''))[-1]
            or os.environ.get('TRAVIS_PULL_REQUEST_BRANCH', '')
            or os.environ.get('TRAVIS_BRANCH', '')
        )
        if branch:
            _note(f"CI workflow detected; using branch '{branch}'", CLR_INFO)

    into.mkdir()
    scm = 'hg' if fetcher == 'hg' else 'git'
    if scm == 'git':
        options = ['--single-branch']
        if branch:
            options += ['--branch', branch]
        if fetcher in {'github', 'gitlab', 'bitbucket'}:
            options += ['--depth', '1']
    elif scm == 'hg':
        options = ['--branch', branch if branch else 'default']
    scm_command = [scm, 'clone', *options, repo, str(into)]
    run_result = subprocess.run(scm_command, capture_output=True, check=False)
    if run_result.returncode != 0:
        _fail(f"Unable to clone:\n  {' '.join(scm_command)}")
        _fail(run_result.stderr.decode())
        return False
    return True


def _branch(recipe: str) -> str | None:
    """Return the recipe's branch if available, else the empty string.
    >>> _branch('(shx :branch "develop" ...)')
    'develop'
    >>> assert _branch('(shx ...)') is None
    """
    tokenized_recipe = _tokenize_expression(recipe)
    if ':branch' not in tokenized_recipe:
        return None
    return tokenized_recipe[tokenized_recipe.index(':branch') + 1].strip('"')


def check_melpa_pr(pr_url: str) -> None:
    """Check a PR on MELPA."""
    _return_code(0)
    match = re.match(MELPA_PR, pr_url)
    assert match
    changed_files = _pr_changed_files(pr_number=match.groups()[0])

    for changed_file in changed_files:
        filename = changed_file['filename']
        if not filename.startswith('recipes/'):
            _note(f"Skipping {filename} (file is not in ./recipes/)")
            continue

        if changed_file['status'] == 'removed':
            _note(f"Skipping {filename} (file was removed in this PR)")
            continue

        recipe = _url_get(changed_file['raw_url'])
        if Path(filename).name != package_name(recipe):
            _fail(f"'{filename}' does not match '{package_name(recipe)}'")
            continue

        try:
            _recipe_struct_elisp(recipe)
        except ChildProcessError as err:
            _fail(f"Invalid recipe in PR `{recipe.strip()}`: {err}")
            continue

        with tempfile.TemporaryDirectory() as tmpdir:
            # package-build prefers the directory to be named after the package:
            elisp_dir = Path(tmpdir) / package_name(recipe)
            if _clone(
                _clone_address(recipe),
                into=elisp_dir,
                branch=_branch(recipe),
                fetcher=_fetcher(recipe),
            ):
                check_containerized_build(recipe, elisp_dir)
                if os.environ.get('EXIST_OK', '').lower() != 'true':
                    check_package_name(package_name(recipe))
                print('\n<!-- PR reviewer footnotes:')
                _note(f"- {_prettify_recipe(recipe)}", CLR_INFO, ':[^ ]+')
                if repo_info := _repo_info_api(_clone_address(recipe)):
                    print(f"- Created: {repo_info.get('created_at', 'N/A')}")
                    print(f"- Updated: {repo_info.get('updated_at', 'N/A')}")
                    print(f"- Watched: {repo_info.get('watchers_count', 'N/A')}")
                if (reminders := _MELPAZOID_ROOT / '_reminders.json').is_file():
                    for pattern, reminder in json.loads(reminders.read_text()).items():
                        if re.search(pattern, recipe):
                            _note(f"- REMINDER: {reminder}", CLR_WARN)
                print('-->\n')


@functools.lru_cache(maxsize=3)  # cached to avoid rate limiting
def _pr_changed_files(pr_number: str) -> list[dict[str, Any]]:
    """Get data from GitHub API."""
    pr_files_url = f"https://api.github.com/repos/melpa/melpa/pulls/{pr_number}/files"
    return list(json.loads(_url_get(pr_files_url)))


def _prettify_recipe(recipe: str) -> str:
    # re: formatting see https://github.com/melpa/melpa/pull/8072
    return ' '.join(recipe.split()).replace(' :', '\n    :')


@functools.lru_cache
def _clone_address(recipe: str) -> str:
    """Fetch the upstream repository URL for the recipe.
    Throw a ChildProcessError if Emacs encounters a problem.
    >>> _clone_address('(shx :repo "riscy/shx-for-emacs" :fetcher github)')
    'https://github.com/riscy/shx-for-emacs'
    >>> _clone_address('(pmdm :fetcher hg :url "https://hg.serna.eu/emacs/pmdm")')
    'https://hg.serna.eu/emacs/pmdm'
    """
    return eval_elisp(
        f"""
        (require 'package-recipe)
        (send-string-to-terminal
          (oref {_recipe_struct_elisp(recipe)} url))
        """
    )


@functools.lru_cache
def _recipe_struct_elisp(recipe: str) -> str:
    """Turn the recipe into a serialized 'package-recipe' object.
    Throw a ChildProcessError if Emacs encounters a problem.
    >>> _recipe_struct_elisp('(melpazoid :fetcher github :repo "xyz")')
    '#s(package-github-recipe "melpazoid" "https://github.com/xyz" "xyz" ...'
    """
    name = package_name(recipe)
    with tempfile.TemporaryDirectory() as tmpdir:
        (Path(tmpdir) / name).write_text(recipe, 'utf-8')
        return eval_elisp(
            f"""
            (require 'package-build)
            (require 'package-recipe)
            (setq package-build-recipes-dir "{tmpdir}")
            (send-string-to-terminal (format "%S" (package-recipe-lookup "{name}")))
            """
        )


def eval_elisp(script: str) -> str:
    """Run an elisp script in a package-build context.
    >>> eval_elisp('(send-string-to-terminal "Hello world")')
    'Hello world'
    >>> eval_elisp("(require 'package-build) (require 'package-recipe)")
    ''
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        for filename, content in _package_build_files().items():
            (Path(tmpdir) / filename).write_text(content, 'utf-8')
        script = f"""(progn (add-to-list 'load-path "{tmpdir}") {script})"""
        result = subprocess.run(
            ['emacs', '--quick', '--batch', '--eval', script],
            capture_output=True,
            check=False,
            text=True,
        )
        if result.returncode != 0:
            raise ChildProcessError(f"Emacs crashed ({result.stderr}) on: {script!r}")
        return str(result.stdout.strip())


@functools.lru_cache
def _package_build_files() -> dict[str, str]:
    """Grab the required package-build files from the MELPA repo."""
    return {
        filename: _url_get(
            'https://raw.githubusercontent.com/melpa/melpa/master/'
            f"package-build/{filename}"
        )
        for filename in [
            'package-build-badges.el',
            'package-build.el',
            'package-recipe-mode.el',
            'package-recipe.el',
        ]
    }


def _check_loop() -> None:
    """Check MELPA recipes and pull requests in a loop."""
    while True:
        try:
            for target in _fetch_targets():
                start = time.perf_counter()
                if is_recipe(target):
                    _note(f"<!-- Checking recipe: {_prettify_recipe(target)} -->")
                    check_melpa_recipe(target)
                elif re.match(MELPA_PR, target):
                    _note(f"<!-- Checking pull request: {target} -->")
                    check_melpa_pr(target)
                if _return_code() != 0:
                    _fail(f'<!-- Failed in {time.perf_counter() - start:.2f}s -->')
                else:
                    _note(f'<!-- Finished in {time.perf_counter() - start:.2f}s -->')
        except KeyboardInterrupt:  # noqa: PERF203
            breakpoint()  # noqa: T100


def _fetch_targets() -> Iterator[str]:
    """Repeatedly yield PR URL's."""
    previous_target = None
    if shutil.which('pbpaste'):
        print('Monitoring the clipboard for recipes and PRs...')
    while True:
        if shutil.which('pbpaste'):
            possible_target = subprocess.check_output('pbpaste').decode()
        else:
            possible_target = input("Enter recipe or URL for MELPA PR: ")
        target = None
        melpa_pr_match = re.match(MELPA_PR, possible_target)
        if melpa_pr_match:
            target = melpa_pr_match.string[: melpa_pr_match.end()]
        elif is_recipe(possible_target):
            target = _prettify_recipe(possible_target)
        if target and target != previous_target:
            previous_target = target
            yield target
        time.sleep(1)


def _argparse_target(target: str) -> str:
    """For near-term backward compatibility this parser just sets env vars."""
    if re.match(MELPA_PR, target):
        os.environ['MELPA_PR_URL'] = target
    elif Path(target).is_file():
        potential_recipe = Path(target).read_text('utf-8')
        if not is_recipe(potential_recipe):
            raise argparse.ArgumentTypeError(f"{target!r} contains an invalid recipe")
        os.environ['RECIPE'] = potential_recipe
    elif Path(target).is_dir():
        os.environ['LOCAL_REPO'] = target
    else:
        raise argparse.ArgumentTypeError(f"{target!r} must be a MELPA PR or local path")
    return target


def _argparse_recipe(recipe: str) -> str:
    """For near-term backward compatibility this parser just sets env vars."""
    if not is_recipe(recipe):
        raise argparse.ArgumentTypeError(f"{recipe!r} must be a valid MELPA recipe")
    os.environ['RECIPE'] = recipe
    return recipe


def _url_get(url: str, retry: int = 3) -> str:
    if not url.startswith(('http://', 'https://')):
        raise ValueError(url)
    try:
        with urllib.request.urlopen(url) as response:  # noqa: S310
            return str(response.read().decode())
    except urllib.error.URLError as err:
        if retry < 1:
            raise
        print(f'Retrying {url} in 10 seconds: {err}')
        time.sleep(10)
        return _url_get(url, retry - 1)


def _url_ok(url: str) -> bool:
    if not url.startswith(('http://', 'https://')):
        raise ValueError(url)
    if ' ' in url:
        return False
    try:
        with urllib.request.urlopen(
            urllib.request.Request(
                url, method='HEAD', headers={'User-Agent': 'Mozilla/5.0'}
            )
        ):
            return True
    except urllib.error.URLError:
        return False


def _main() -> None:
    parser = argparse.ArgumentParser()
    target_help = 'a recipe, a path to a recipe or package, or a MELPA PR URL'
    parser.add_argument('target', help=target_help, nargs='?', type=_argparse_target)
    parser.add_argument('--license', help='only check licenses', action='store_true')
    parser.add_argument('--recipe', help='a valid MELPA recipe', type=_argparse_recipe)
    pargs = parser.parse_args()

    if pargs.license:
        if not os.environ.get('RECIPE'):
            _fail('Set a recipe using `target` or with: [--recipe RECIPE]')
            return
        check_license(os.environ['RECIPE'])
    elif 'MELPA_PR_URL' in os.environ:
        check_melpa_pr(os.environ['MELPA_PR_URL'])
    elif 'RECIPE' in os.environ:
        check_melpa_recipe(os.environ['RECIPE'])
    elif 'LOCAL_REPO' in os.environ:
        _fail('Set a recipe with: [--recipe RECIPE]')
    else:
        _check_loop()


if __name__ == '__main__':
    _main()
    sys.exit(_return_code())
