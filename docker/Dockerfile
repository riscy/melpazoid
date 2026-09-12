# Based on:
# <https://gist.github.com/dvmarinoff/f9000e3cce3fd5e515965beebe01cdbf>
FROM ubuntu:26.04

ARG DEBIAN_FRONTEND=noninteractive
ENV EMACS_VERSION=31.1

RUN apt-get update \
        && apt-get -y --no-install-recommends install \
          ca-certificates curl gnupg openssh-client wget \
          autoconf gcc make pkg-config texinfo \
          libxml2-dev libjansson-dev libncurses-dev libgnutls28-dev zlib1g-dev \
        && curl -fsSL https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.gz | tar -xz -C ./ \
        && cd ./emacs-${EMACS_VERSION} \
        && ./configure --without-native-compilation --without-x \
        && make -j2 install \
        && rm -rf ./emacs-${EMACS_VERSION} \
        && apt-get -y autoremove \
        && rm -rf /var/lib/apt/lists/* /root/.cache/*

# install any special system-level packages we depend on (e.g. cmake):
ARG NATIVE_DEPS=""
ENV NATIVE_DEPS="${NATIVE_DEPS}"
RUN [ -n "$NATIVE_DEPS" ] && apt-get update && apt-get -y install ${NATIVE_DEPS} || :

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
CMD ["emacs", "--no-site-file", "--batch", "--load=melpazoid.el"]
