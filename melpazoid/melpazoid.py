# -*- coding: utf-8 -*-
"""
usage: melpazoid.py [-h] [--license] [--recipe RECIPE] [target]

positional arguments:
  target           a recipe, a path to a recipe or package, or a MELPA PR URL

optional arguments:
  -h, --help       show this help message and exit
  --license        only check licenses
  --recipe RECIPE  a valid MELPA recipe
"""
__author__ = 'Chris Rayner <dchrisrayner@gmail.com>'
__license__ = 'SPDX-License-Identifier: GPL-3.0-or-later'
import argparse
import configparser
import functools
import json
import operator
import os
import pdb
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.request
from typing import Any, Dict, Iterator, List, Optional, Set, TextIO

_RETURN_CODE = 0  # eventual return code when run as script
_MELPAZOID_ROOT = os.path.join(
    os.path.dirname(__file__) if '__file__' in vars() else os.getcwd(),
    '..',
)

# define the colors of the report (or none), per https://no-color.org
# https://misc.flogisoft.com/bash/tip_colors_and_formatting
NO_COLOR = os.environ.get('NO_COLOR', False)
CLR_OFF = '' if NO_COLOR else '\033[0m'
CLR_ERROR = '' if NO_COLOR else '\033[31m'
CLR_WARN = '' if NO_COLOR else '\033[33m'
CLR_INFO = '' if NO_COLOR else '\033[32m'

MELPA_PR = r'https?://github.com/melpa/melpa/pull/([0-9]+)'


def _return_code(return_code: Optional[int] = None) -> int:
    """Return (and optionally set) the current return code.
    If return_code matches env var EXPECT_ERROR, return 0 --
    this is useful for running CI checks on melpazoid itself.
    """
    global _RETURN_CODE  # pylint: disable=global-statement
    if return_code is not None:
        _RETURN_CODE = return_code
    expect_error = int(os.environ.get('EXPECT_ERROR', 0))
    return 0 if _RETURN_CODE == expect_error else _RETURN_CODE


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


def check_containerized_build(recipe: str, elisp_dir: str) -> None:
    """Build a Docker container to run checks on elisp_dir, given a recipe."""
    if not is_recipe(recipe):
        _fail(f"Not a valid recipe: {recipe}")
        return

    files = [os.path.relpath(f, elisp_dir) for f in _files_in_recipe(recipe, elisp_dir)]
    elisp_files = [file_ for file_ in files if file_.endswith('.el')]
    if len({os.path.basename(file_) for file_ in elisp_files}) != len(elisp_files):
        _fail(f"Multiple elisp files share the same basename: {', '.join(files)}")
        return

    pkg_dir = os.path.join(_MELPAZOID_ROOT, 'pkg')
    shutil.rmtree(pkg_dir, ignore_errors=True)
    _note(f"<!-- Building container for {package_name(recipe)}... 🐳 -->")
    for ii, file in enumerate(files):
        target = os.path.basename(file) if file.endswith('.el') else file
        target = os.path.join(pkg_dir, target)
        os.makedirs(os.path.dirname(target), exist_ok=True)
        # shutil.copy/copytree won't work here because file can be a file or a dir:
        subprocess.run(['cp', '-r', os.path.join(elisp_dir, file), target], check=True)
        files[ii] = target
    _write_requirements(files, recipe)
    cmd = ['make', '-C', _MELPAZOID_ROOT, 'test']
    main_file = _main_file(files, recipe)
    if main_file and sum(f.endswith('.el') for f in os.listdir(pkg_dir)) > 1:
        cmd.append(f"PACKAGE_MAIN={os.path.basename(main_file)}")
    run_result = subprocess.run(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False
    )
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
    print()
    print_packaging(recipe, elisp_dir)


def _files_in_recipe(recipe: str, elisp_dir: str) -> List[str]:
    """Return a file listing, relative to elisp_dir.
    >>> _files_in_recipe('(melpazoid :fetcher github :repo "xyz")', 'melpazoid')
    ['melpazoid/melpazoid.el']
    """
    files = eval_elisp(
        f"""
        (require 'package-build)
        (setq package-build-working-dir "{os.path.dirname(elisp_dir)}")
        (setq rcp {_recipe_struct_elisp(recipe)})
        (send-string-to-terminal
            (mapconcat #'car (package-build-expand-files-spec rcp t) "\n"))
        """
    ).split('\n')
    files = [os.path.join(elisp_dir, file) for file in files]
    return sorted(file for file in files if os.path.exists(file))


def _files_in_default_recipe(recipe: str, elisp_dir: str) -> List[str]:
    try:
        return _files_in_recipe(_default_recipe(recipe), elisp_dir)
    except ChildProcessError:
        # It is possible that the default recipe is completely invalid and
        # will throw an error -- in that case, just return the empty list:
        return []


def _default_recipe(recipe: str) -> str:
    """Simplify the given recipe, usually to the default.
    >>> _default_recipe('(recipe :repo "a/b" :fetcher hg :branch na :files ("*.el"))')
    '(recipe :repo "a/b" :fetcher hg :branch na)'
    >>> _default_recipe('(recipe :fetcher hg :url "a/b")')
    '(recipe :url "a/b" :fetcher hg)'
    """
    tokens = _tokenize_expression(recipe)
    fetcher = tokens.index(':fetcher')
    repo = tokens.index(':repo' if ':repo' in tokens else ':url')
    indices = [1, repo, repo + 1, fetcher, fetcher + 1]
    if ':branch' in tokens:
        branch = tokens.index(':branch')
        indices += [branch, branch + 1]
    return '(' + ' '.join(operator.itemgetter(*indices)(tokens)) + ')'


def _tokenize_expression(expression: str) -> List[str]:
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
    for token in tokens:
        if token == '(':
            unbalanced_parens += 1
        if token == ')':
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


def _main_file(files: List[str], recipe: str) -> Optional[str]:
    """Figure out the 'main' file of the recipe, per MELPA convention.
    >>> _main_file(['pkg/a.el', 'pkg/b.el'], '(a :files ...)')
    'pkg/a.el'
    >>> _main_file(['a.el', 'b.el'], '(b :files ...)')
    'b.el'
    >>> _main_file(['a.el', 'a-pkg.el'], '(a :files ...)')
    'a-pkg.el'
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
            if os.path.basename(el)
            in (f"{name}-pkg.el", f"{name}-pkg.el.in", f"{name}.el")
        )
    except StopIteration:
        return None


def _write_requirements(files: List[str], recipe: str) -> None:
    """Create a little elisp script that Docker will run as setup."""
    with open('_requirements.el', 'w', encoding='utf-8') as requirements_el:
        requirements_el.write(
            f'''
            ;; {time.strftime('%Y-%m-%d')} ; helps to invalidate old Docker cache
            ;; NOTE: emacs --script <file.el> will set `load-file-name' to <file.el>
            ;; which can disrupt the compilation of packages that use that variable:
            (setq load-file-name nil)
            ;; (setq network-security-level 'low)  ; expired certs last resort
            (require 'package)
            (package-initialize)
            (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
            (package-refresh-contents)
            (package-install 'pkg-info)
            (package-install 'package-lint)
            '''
        )
        for req in requirements(files, recipe):
            req, *version_maybe = req.split()
            version = version_maybe[0].strip('"') if version_maybe else 'N/A'
            if req == 'emacs':
                continue
            if '"' in req:  # common failure mode: misplaced quotation marks
                _fail(f"- Invalid dependency in Package-Requires: `{req}`")
                continue
            if req == 'marginalia':
                _fail(
                    "- Don't require marginalia: https://github.com/minad/marginalia#adding-custom-annotators-or-classifiers"
                )
            # always install the latest available version of the dependency.
            # ignore-errors because package-lint handles checking the index
            requirements_el.write(fr'(message "Installing {req} {version}\n")')
            requirements_el.write(
                f"(ignore-errors (package-install (cadr (assq '{req} package-archive-contents))))\n"
            )


def requirements(files: List[str], recipe: Optional[str] = None) -> Set[str]:
    """Return (downcased) requirements given a listing of files.
    If a recipe is given, use it to determine which file is the main file;
    otherwise scan every .el file for requirements.
    """
    reqs = []
    if recipe:
        main_file = _main_file(files, recipe)
        if main_file:
            files = [main_file]
    for filename in (f for f in files if os.path.isfile(f)):
        if filename.endswith('-pkg.el'):
            with open(filename, encoding='utf-8', errors='replace') as pkg_el:
                reqs.append(_reqs_from_pkg_el(pkg_el))
        elif filename.endswith('.el'):
            with open(filename, encoding='utf-8', errors='replace') as el_file:
                reqs.append(_reqs_from_el_file(el_file) or '')
    reqs = sum((re.split('[()]', req) for req in reqs), [])
    return {req.replace(')', '').strip().lower() for req in reqs if req.strip()}


def _reqs_from_pkg_el(pkg_el: TextIO) -> str:
    """Pull the requirements out of a -pkg.el file.
    >>> import io
    >>> _reqs_from_pkg_el(io.StringIO(
    ...   '''(define-package "x" "1.2" "A pkg." '(a (b "31.5")))'''))
    '( a ( b "31.5" ) )'
    """
    # TODO: fails if EXTRA-PROPERTIES args were given to #'define-package
    reqs = pkg_el.read()
    reqs = ' '.join(_tokenize_expression(reqs)[5:-1])
    reqs = reqs[reqs.find("' (") + 2 :]
    reqs = reqs[: reqs.find(') )') + 3]
    return reqs


def _reqs_from_el_file(el_file: TextIO) -> Optional[str]:
    """Hacky function to pull the requirements out of an elisp file.
    >>> import io
    >>> _reqs_from_el_file(io.StringIO(';; package-requires: ((emacs "24.4"))'))
    '((emacs "24.4"))'
    """
    for line in el_file.readlines():
        match = re.match(r'[; ]*Package-Requires[ ]*:[ ]*(.*)$', line, re.I)
        if match:
            try:
                _tokenize_expression(match.groups()[0])
            except ValueError as err:
                _fail(str(err))
            return match.groups()[0].strip()
    return None


def _check_license_api(clone_address: str) -> bool:
    """Use the GitHub or GitLab API to check for a license.
    Prints out the particular license as a side effect.
    Return False if unable to check (e.g. it's not on GitHub).
    >>> _check_license_api('https://github.com/riscy/elfmt')
    - GNU General Public License v3.0 (via API)
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
        # 'GNU General Public License v2.0',  # https://github.com/johannes-mueller/company-wordfreq.el/issues/6
        'GNU General Public License v3.0',
        'GNU Lesser General Public License v3.0',
        'ISC License',
        'MIT License',
        'Mozilla Public License 2.0',
        'The Unlicense',
    }
    print(f"- {license_.get('name')} (via API)")
    if license_.get('name') in gpl_compatible_licensee_licenses:
        pass
    elif license_.get('name') == 'Other':
        _note('  - Try to use a standard license file format', CLR_WARN)
        print('    See: https://github.com/licensee/licensee')
    else:
        _note("  - License not in melpazoid's recognized list", CLR_WARN)
    return True


@functools.lru_cache()
def _repo_info_api(clone_address: str) -> Optional[Dict[str, Any]]:
    """Use the GitHub or GitLab API to fetch details about a repository.
    Raise urllib.error.URLError if API request fails.
    """
    repo_info: Dict[str, Any]
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
        projects_api = 'https://gitlab.com/api/v4/projects'
        repo_info = json.loads(_url_get(f"{projects_api}/{project_id}?license=true"))
        # HACK: align GitLab API response with typical GitHub api response
        repo_info['updated_at'] = repo_info['last_activity_at']
        repo_info['watchers_count'] = repo_info['star_count']
        return repo_info

    return None


def _check_license_file(elisp_dir: str) -> None:
    """Scan any COPYING or LICENSE files."""
    license_names = (
        'copying',
        'copying.txt',
        'license',
        'license.txt',
        'license.md',
        'unlicense',
    )
    for license_ in os.scandir(elisp_dir):
        if license_.name.lower() in license_names:
            with open(license_.path, encoding='utf-8', errors='replace') as stream:
                print(f"- {license_.name} excerpt: `{stream.readline().strip()}...`")
            return
    _fail('- Add a GPL-compatible LICENSE file to the repository')


def _check_file_for_license_boilerplate(el_file: TextIO) -> Optional[str]:
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
        license_ = _spdx_license(license_id=match.groups()[0])
        if license_ is None:
            _fail(f"- Invalid SPDX license: {match.groups()[0]}")
            return None
        if not license_['isFsfLibre']:
            _fail(f"Not free/libre: {match.groups()[0]}")
        return str(license_['name'])

    gpl_compatible_license_excerpts = {
        # NOTE: consider using https://github.com/emacscollective/elx instead
        'Apache License 2.0': 'Licensed under the Apache License, Version 2.0',
        'BSD*': 'Redistribution and use in source and binary forms',
        'FSFAP': 'Copying and distribution of this file, with or without',
        'GPL*': 'is free software.* you can redistribute it',
        'ISC License': 'Permission to use, copy, modify, and/or distribute this',
        'MIT License': 'Permission is hereby granted, free of charge, to any person',
        'The Unlicense': 'This is free and unencumbered software released into',
    }
    for license_key, license_text in gpl_compatible_license_excerpts.items():
        if re.search(license_text, text):
            return license_key
    return None


@functools.lru_cache()
def _spdx_license(license_id: str) -> Optional[Dict[str, Any]]:
    license_id = license_id.replace(' ', '-')
    try:
        response = _url_get(f'https://spdx.org/licenses/{license_id.strip()}.json')
        return dict(json.loads(response))
    except urllib.error.HTTPError:
        return None


def print_packaging(recipe: str, elisp_dir: str) -> None:
    """Print additional details (how it's licensed, what files, etc.)"""
    _note('Package and license:', CLR_INFO)
    _check_recipe(recipe, elisp_dir)
    _check_package_requires(recipe, elisp_dir)
    _check_license(recipe, elisp_dir)
    print()


def _check_license(recipe: str, elisp_dir: str) -> None:
    clone_address = _clone_address(recipe)
    if not _check_license_api(clone_address):
        _check_license_file(elisp_dir)
    for file in _files_in_recipe(recipe, elisp_dir):
        relpath = os.path.relpath(file, elisp_dir)
        if os.path.isdir(file):
            print(f"- {relpath} -- directory")
            continue
        if not file.endswith('.el'):
            print(f"- {relpath} -- not elisp")
            continue
        if file.endswith('-pkg.el'):
            _note(f"- {relpath} -- consider excluding; MELPA creates one", CLR_WARN)
            continue
        with open(file, encoding='utf-8', errors='replace') as stream:
            try:
                header = stream.readline()
                header = header.split('-*-')[0]
                header = header.split(' --- ')[1]
                header = header.strip()
            except IndexError:
                header = f"{CLR_ERROR}(no header){CLR_OFF}"
                _return_code(2)
            boilerplate = _check_file_for_license_boilerplate(stream)
            print(
                f"- {boilerplate or 'unknown license'} -- {relpath}"
                + (f": {header}" if header else "")
            )
            if boilerplate is None:
                _fail(
                    '- Add *formal* license boilerplate or an'
                    ' [SPDX-License-Identifier](https://spdx.dev/ids/)'
                    f" to {relpath}"
                )


def _check_recipe(recipe: str, elisp_dir: str) -> None:
    files = _files_in_recipe(recipe, elisp_dir)
    if ':branch' in recipe:
        _note('- Avoid specifying `:branch` except in unusual cases', CLR_WARN)
    if not _main_file(files, recipe):
        _fail(f"- No .el file matches the name '{package_name(recipe)}'")
    if ':url' in recipe and 'https://github.com' in recipe:
        _fail('- Use `:fetcher github :repo <repo>` instead of `:url`')
    if ':files' in recipe:
        if files == _files_in_default_recipe(recipe, elisp_dir):
            _note(f"- Prefer equivalent recipe: `{_default_recipe(recipe)}`", CLR_WARN)
        elif ':defaults' not in recipe:
            _note('- Prefer the default recipe or `:defaults`, if possible.', CLR_WARN)


def _check_package_requires(recipe: str, elisp_dir: str) -> None:
    """Print the list of Package-Requires from the 'main' file.
    Report on any mismatches between this file and other files, since the ones
    in the other files will be ignored.
    """
    files = _files_in_recipe(recipe, elisp_dir)
    main_file = _main_file(files, recipe)
    if not main_file:
        _fail("- Can't check package-requires if there is no 'main' file")
        return
    main_requirements = requirements(files, recipe)
    for file in files:
        file_requirements = requirements([file])
        if file_requirements and file_requirements > main_requirements:
            _fail(
                f"- Package-Requires mismatch between {os.path.basename(file)} and "
                f"{os.path.basename(main_file)}!"
            )


def check_package_name(name: str) -> None:
    """Print list of similar, or at least similarly named, packages.
    Report any occurrences of invalid/reserved package names.
    """
    keywords = [name]
    keywords += [re.sub(r'[0-9]', '', name)]
    keywords += [name[:-5]] if name.endswith('-mode') else []
    keywords += [f"{name.split('-')[0]}-"] if '-' in name else []
    keywords += ['org-' + name[3:]] if name.startswith('ox-') else []
    keywords += ['ox-' + name[4:]] if name.startswith('org-') else []
    known_names = emacsattic_packages(*keywords)
    known_names.update(emacswiki_packages(*keywords))
    known_names.update(emacsmirror_packages())
    known_names.update(elpa_packages(*keywords))
    known_names.update(melpa_packages(*keywords))
    similar_names = [
        (name_, url)
        for name_, url in known_names.items()
        if any(name_.startswith(keyword) for keyword in keywords)
    ][:10]
    try:
        eval_elisp(f"(require '{name})")
        name_builtin = True
    except ChildProcessError:
        name_builtin = False
    reserved_names = ('^git-rebase$', '^helm-source-', '^ob-', '^ox-')
    name_reserved = any(re.match(reserved, name) for reserved in reserved_names)
    if not similar_names and not name_builtin and not name_reserved:
        return

    _note('Package name:', CLR_INFO)
    if name_builtin:
        _fail(f"- Error: `{name}` is an Emacs builtin\n", highlight='Error')
        return
    if name in known_names:
        _fail(f"- Error: `{name}` is taken: {known_names[name]}\n", highlight='Error')
        return
    if name_reserved:
        _fail(f"- Error: `{name}` is reserved\n", highlight='Error')
        return
    for name_, url in similar_names:
        print(f"- `{name_}` is similar: {url}")
    print()


@functools.lru_cache()
def emacsattic_packages(*keywords: str) -> Dict[str, str]:
    """(Obsolete) packages on Emacsattic matching 'keywords'.
    >>> emacsattic_packages('sos')
    {'sos': 'https://github.com/emacsattic/sos'}
    """
    packages = {kw: f"https://github.com/emacsattic/{kw}" for kw in keywords}
    return {kw: url for kw, url in packages.items() if _url_ok(url)}


@functools.lru_cache()
def emacswiki_packages(*keywords: str) -> Dict[str, str]:
    """Packages on emacswiki.org mirror matching 'keywords'.
    >>> emacswiki_packages('rss')
    {'rss': 'https://github.com/emacsmirror/emacswiki.org/blob/master/rss.el'}
    """
    packages = {}
    for keyword in set(keywords):
        el_file = keyword if keyword.endswith('.el') else (keyword + '.el')
        pkg = f"https://github.com/emacsmirror/emacswiki.org/blob/master/{el_file}"
        if _url_ok(pkg):
            packages[keyword] = pkg
    return packages


@functools.lru_cache()
def emacsmirror_packages() -> Dict[str, str]:
    """All mirrored packages."""
    epkgs = 'https://raw.githubusercontent.com/emacsmirror/epkgs/master/.gitmodules'
    epkgs_parser = configparser.ConfigParser()
    epkgs_parser.read_string(_url_get(epkgs))
    return {
        epkg.split('"')[1]: data['url'] + ' (via emacsmirror)'
        for epkg, data in epkgs_parser.items()
        if epkg != 'DEFAULT'
    }


@functools.lru_cache()
def elpa_packages(*keywords: str) -> Dict[str, str]:
    """ELPA packages matching keywords.
    >>> elpa_packages('ahungry-theme')
    {'ahungry-theme': 'https://elpa.gnu.org/packages/ahungry-theme.html'}
    >>> elpa_packages('git-modes')
    {'git-modes (nongnu)': 'https://elpa.nongnu.org/nongnu/git-modes.html'}
    """
    # q.v. http://elpa.gnu.org/packages/archive-contents
    elpa = 'https://elpa.gnu.org'
    nongnu_elpa = 'https://elpa.nongnu.org'
    sources = {
        **{kw: f"{elpa}/devel/{kw}.html" for kw in keywords},
        **{kw: f"{elpa}/packages/{kw}.html" for kw in keywords},
        **{f"{kw} (nongnu)": f"{nongnu_elpa}/nongnu/{kw}.html" for kw in keywords},
    }
    return {kw: url for kw, url in sources.items() if _url_ok(url)}


@functools.lru_cache()
def melpa_packages(*keywords: str) -> Dict[str, str]:
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
    with tempfile.TemporaryDirectory() as elisp_dir:
        # package-build prefers the directory to be named after the package:
        elisp_dir = os.path.join(elisp_dir, package_name(recipe))
        clone_address = _clone_address(recipe)
        if _local_repo():
            print(f"Using local repository at {_local_repo()}")
            shutil.copytree(_local_repo(), elisp_dir)
            check_containerized_build(recipe, elisp_dir)
        elif _clone(clone_address, elisp_dir, _branch(recipe), _fetcher(recipe)):
            check_containerized_build(recipe, elisp_dir)


def check_license(recipe: str) -> None:
    """Check license for a given recipe."""
    # TODO: DRY up wrt check_melpa_recipe
    _return_code(0)
    with tempfile.TemporaryDirectory() as elisp_dir:
        # package-build prefers the directory to be named after the package:
        elisp_dir = os.path.join(elisp_dir, package_name(recipe))
        clone_address = _clone_address(recipe)
        if _local_repo():
            print(f"Using local repository at {_local_repo()}")
            shutil.copytree(_local_repo(), elisp_dir)
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


def _local_repo() -> str:
    local_repo = os.path.expanduser(os.environ.get('LOCAL_REPO', ''))
    assert not local_repo or os.path.isdir(local_repo)
    return local_repo


def _clone(repo: str, into: str, branch: Optional[str], fetcher: str) -> bool:
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

    os.makedirs(into)
    scm = 'hg' if fetcher == 'hg' else 'git'
    if scm == 'git':
        options = ['--single-branch']
        if branch:
            options += ['--branch', branch]
        if fetcher in {'github', 'gitlab', 'bitbucket'}:
            options += ['--depth', '1']
    elif scm == 'hg':
        options = ['--branch', branch if branch else 'default']
    scm_command = [scm, 'clone', *options, repo, into]
    run_result = subprocess.run(
        scm_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False
    )
    if run_result.returncode != 0:
        _fail(f"Unable to clone:\n  {' '.join(scm_command)}")
        _fail(run_result.stderr.decode())
        return False
    return True


def _branch(recipe: str) -> Optional[str]:
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
            _note(f"Skipping {filename} (not a recipe)")
            continue

        recipe = _url_get(changed_file['raw_url'])
        if os.path.basename(filename) != package_name(recipe):
            _fail(f"'{filename}' does not match '{package_name(recipe)}'")
            continue

        with tempfile.TemporaryDirectory() as tmpdir:
            # package-build prefers the directory to be named after the package:
            elisp_dir = os.path.join(tmpdir, package_name(recipe))
            if _clone(
                _clone_address(recipe),
                into=elisp_dir,
                branch=_branch(recipe),
                fetcher=_fetcher(recipe),
            ):
                check_containerized_build(recipe, elisp_dir)
                if os.environ.get('EXIST_OK', '').lower() != 'true':
                    check_package_name(package_name(recipe))
                print('<!-- Footnotes:')
                _note(f"- {_prettify_recipe(recipe)}", CLR_INFO, ':[^ ]+')
                repo_info = _repo_info_api(_clone_address(recipe))
                if repo_info:
                    if repo_info.get('archived'):
                        _fail('- GitHub repository is archived')
                    print(f"- Created: {repo_info.get('created_at', 'N/A')}")
                    print(f"- Updated: {repo_info.get('updated_at', 'N/A')}")
                    print(f"- Watched: {repo_info.get('watchers_count', 'N/A')}")
                print('-->\n')


@functools.lru_cache(maxsize=3)  # cached to avoid rate limiting
def _pr_changed_files(pr_number: str) -> List[Dict[str, Any]]:
    """Get data from GitHub API."""
    melpa_pull_api = 'https://api.github.com/repos/melpa/melpa/pulls'
    return list(json.loads(_url_get(f"{melpa_pull_api}/{pr_number}/files")))


def _prettify_recipe(recipe: str) -> str:
    # re: formatting see https://github.com/melpa/melpa/pull/8072
    return ' '.join(recipe.split()).replace(' :', '\n    :')


@functools.lru_cache()
def _clone_address(recipe: str) -> str:
    """Fetch the upstream repository URL for the recipe.
    >>> _clone_address('(shx :repo "riscy/shx-for-emacs" :fetcher github)')
    'https://github.com/riscy/shx-for-emacs.git'
    >>> _clone_address('(pmdm :fetcher hg :url "https://hg.serna.eu/emacs/pmdm")')
    'https://hg.serna.eu/emacs/pmdm'
    """
    return eval_elisp(
        f"""
        (require 'package-recipe)
        (send-string-to-terminal
          (package-recipe--upstream-url {_recipe_struct_elisp(recipe)}))
        """
    )


@functools.lru_cache()
def _recipe_struct_elisp(recipe: str) -> str:
    """Turn the recipe into a serialized 'package-recipe' object.
    Throw a ChildProcessError if Emacs encounters a problem.
    >>> _recipe_struct_elisp('(melpazoid :fetcher github :repo "xyz")')
    '#s(package-github-recipe "melpazoid" nil "xyz" nil nil nil nil nil nil nil nil)'
    """
    name = package_name(recipe)
    with tempfile.TemporaryDirectory() as tmpdir:
        with open(os.path.join(tmpdir, name), 'w', encoding='utf-8') as file:
            file.write(recipe)
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
            with open(os.path.join(tmpdir, filename), 'w', encoding='utf-8') as file:
                file.write(content)
        script = f"""(progn (add-to-list 'load-path "{tmpdir}") {script})"""
        result = subprocess.run(
            ['emacs', '--quick', '--batch', '--eval', script],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
        if result.returncode != 0:
            raise ChildProcessError(f"Emacs crashed: {result.stderr.decode()}")
        return str(result.stdout.decode()).strip()


@functools.lru_cache()
def _package_build_files() -> Dict[str, str]:
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
                if is_recipe(target):
                    _note(f"<!-- Checking recipe: {_prettify_recipe(target)} -->")
                    check_melpa_recipe(target)
                elif re.match(MELPA_PR, target):
                    _note(f"<!-- Checking pull request: {target} -->")
                    check_melpa_pr(target)
                if _return_code() != 0:
                    _fail('<!-- Issues detected -->')
                else:
                    _note('<!-- Finished -->', CLR_INFO)
        except KeyboardInterrupt:
            pdb.set_trace()  # pylint: disable=forgotten-debug-statement


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
    elif os.path.isfile(target):
        with open(target, encoding='utf-8') as file:
            potential_recipe = file.read()
        if not is_recipe(potential_recipe):
            raise argparse.ArgumentTypeError(f"{target!r} contains an invalid recipe")
        os.environ['RECIPE'] = potential_recipe
    elif os.path.isdir(target):
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


def _url_get(url: str) -> str:
    with urllib.request.urlopen(url) as response:
        return str(response.read().decode())


def _url_ok(url: str) -> bool:
    try:
        with urllib.request.urlopen(urllib.request.Request(url, method='HEAD')):
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
        else:
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
