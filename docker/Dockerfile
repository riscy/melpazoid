FROM ubuntu:20.04

# required to silently install libxml2-dev
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
        && apt-get -y --no-install-recommends install software-properties-common \
        && apt-add-repository -y ppa:kelleyk/emacs \
        && apt-get -y --no-install-recommends install emacs28 \
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
CMD ["emacs", "--script", "melpazoid.el"]
