# Based on https://github.com/JAremko/docker-emacs

ARG VERSION=latest
FROM ubuntu:$VERSION

# Fix "Couldn't register with accessibility bus" error message
ENV NO_AT_BRIDGE=1

ENV DEBIAN_FRONTEND noninteractive

# basic stuff
RUN echo 'APT::Get::Assume-Yes "true";' >> /etc/apt/apt.conf \
    && apt-get update && apt-get install \
    bash \
    build-essential \
    git \
    gzip \
    language-pack-en-base \
    libgl1-mesa-glx \
    make \
    sudo \
    tar \
    unzip \
# Cleanup
    && apt-get purge build-essential \
    && apt-get autoremove \
    && rm -rf /tmp/* /var/lib/apt/lists/* /root/.cache/*

# Emacs
RUN apt-get update && apt-get install software-properties-common \
    && apt-add-repository ppa:kelleyk/emacs \
    && apt-get update && apt-get install emacs26 \
# Cleanup
    && apt-get purge software-properties-common \
    && rm -rf /tmp/* /var/lib/apt/lists/* /root/.cache/*

ENV WORKSPACE "/workspace"
ENV ELISP_PATH "${WORKSPACE}/_elisp"

RUN useradd emacser -d $WORKSPACE
RUN mkdir -p $ELISP_PATH && chown -R emacser $WORKSPACE
USER emacser:emacser

COPY docker/requirements.el $WORKSPACE
RUN emacs --script $WORKSPACE/requirements.el

COPY _requirements.el $WORKSPACE
RUN emacs --script $WORKSPACE/_requirements.el

COPY --chown=emacser:emacser docker/.emacs $WORKSPACE
COPY --chown=emacser:emacser _elisp $ELISP_PATH
COPY --chown=emacser:emacser melpazoid/melpazoid.el $ELISP_PATH

ARG PACKAGE_MAIN
ENV PACKAGE_MAIN "${PACKAGE_MAIN}"

WORKDIR $ELISP_PATH
CMD ["/usr/bin/emacs", "--script", "melpazoid.el"]
