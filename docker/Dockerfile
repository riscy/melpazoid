# Based on:
# <https://ubuntuhandbook.org/index.php/2023/08/gnu-emacs-29-1-ubuntu-ppa/>
FROM ubuntu:22.04

RUN apt-get update \
        && apt-get -y install curl gnupg openssh-client wget \
        && apt-get -y --no-install-recommends install software-properties-common \
        && add-apt-repository ppa:ubuntuhandbook1/emacs \
        && apt-get -y install emacs emacs-common \
        && apt-get -y purge software-properties-common \
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
