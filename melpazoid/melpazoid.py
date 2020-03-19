import configparser
    known_packages = _known_packages()
    known_names = [
        name
        for name in known_packages
        if name in _package_name(recipe) or _package_name(recipe) in name
    if not known_names:
        return
    _note('\n### Similarly named packages ###\n', CLR_INFO)
    for name in known_names:
        if name == _package_name(recipe):
            _fail(f"- {name}: {known_packages[name]} (name conflict)")
        else:
            print(f"- {name}: {known_packages[name]}")
def _known_packages() -> dict:
    melpa_packages = {
        package: f"https://melpa.org/#/{package}"
    epkgs = 'https://raw.githubusercontent.com/emacsmirror/epkgs/master/.gitmodules'
    epkgs_parser = configparser.ConfigParser()
    epkgs_parser.read_string(requests.get(epkgs).text)
    epkgs_packages = {
        epkg.split('"')[1]: epkgs_parser[epkg]['url']
        for epkg in epkgs_parser
        if epkg != 'DEFAULT'
    }
    return {**epkgs_packages, **melpa_packages}
        if _clone(clone_address, into=elisp_dir, branch=_branch(recipe), scm=scm):
def _clone(repo: str, into: str, branch: str = None, scm: str = 'git') -> bool:
    print(f"Checking out {repo}")
        _fail(f"Unable to locate {repo}")
        if _clone(clone_address, into=elisp_dir, branch=_branch(recipe)):