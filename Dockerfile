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
    dbus-x11 \
    fontconfig \
    git \
    gzip \
    language-pack-en-base \
    libgl1-mesa-glx \
    make \
    sudo \
    tar \
    unzip \
# su-exec
    && git clone https://github.com/ncopa/su-exec.git /tmp/su-exec \
    && cd /tmp/su-exec \
    && make \
    && chmod 770 su-exec \
    && mv ./su-exec /usr/local/sbin/ \
# Cleanup
    && apt-get purge build-essential \
    && apt-get autoremove \
    && rm -rf /tmp/* /var/lib/apt/lists/* /root/.cache/*

# Emacs
RUN apt-get update && apt-get install software-properties-common \
    && apt-add-repository ppa:kelleyk/emacs \
    && apt-get update && echo hi && apt-get install emacs26 \
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
RUN emacs --script ~/_requirements.el

COPY --chown=emacser:emacser docker/.emacs $WORKSPACE
COPY --chown=emacser:emacser _elisp $ELISP_PATH
COPY --chown=emacser:emacser melpazoid.el $ELISP_PATH

ARG PACKAGE_NAME
ENV PACKAGE_NAME "${PACKAGE_NAME}"

WORKDIR $ELISP_PATH
CMD ["/usr/bin/emacs", "--script", "melpazoid.el"]
