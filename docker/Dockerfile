# With big thanks to @Silex and @purcell, this Dockerfile is based on:
# <https://github.com/Silex/docker-emacs/blob/master/29.1/debian/Dockerfile>

FROM nixos/nix:2.19.1 AS nixos

ADD https://api.github.com/repos/purcell/nix-emacs-ci/git/refs/heads/master /tmp/cache
RUN nix-env -iA cachix -f https://cachix.org/api/v1/install \
    && cachix use emacs-ci \
    && nix-env -iA emacs-29-1 -f https://github.com/purcell/nix-emacs-ci/archive/master.tar.gz \
    && nix --extra-experimental-features nix-command copy --no-require-sigs --to /nix-emacs "$(command -v emacs)" \
    && ln --relative --symbolic /nix-emacs/nix/store/*emacs* /nix-emacs/nix/store/emacs

FROM debian:trixie-20231120-slim

COPY --from=nixos /nix-emacs/nix/store /nix/store
ENV PATH="/nix/store/emacs/bin:$PATH"

RUN apt-get update \
        && apt-get -y install curl gnupg openssh-client wget \
        && apt-get -y autoremove \
        && rm -rf /var/lib/apt/lists/* /root/.cache/*

ENV WORKSPACE "/workspace"
ENV ELISP_PATH "${WORKSPACE}/pkg"

RUN useradd emacser -d $WORKSPACE \
        && mkdir -p $ELISP_PATH \
        && chown -R emacser $WORKSPACE
USER emacser:emacser

COPY _requirements.el $WORKSPACE
RUN emacs --script $WORKSPACE/_requirements.el

COPY --chown=emacser:emacser docker/.emacs $WORKSPACE
COPY --chown=emacser:emacser pkg $ELISP_PATH
COPY --chown=emacser:emacser melpazoid/melpazoid.el $ELISP_PATH

ARG PACKAGE_MAIN
ENV PACKAGE_MAIN "${PACKAGE_MAIN}"

WORKDIR $ELISP_PATH
CMD ["emacs", "--quick", "--batch", "--load=melpazoid.el"]
