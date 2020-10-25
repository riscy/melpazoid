FROM ubuntu:20.04

RUN apt-get update \
        && apt-get install -y --no-install-recommends make tar gzip git curl ca-certificates gcc libgnutls28-dev libjansson-dev libncurses-dev \
        && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends libxml2-dev \
        && curl -O https://ftp.gnu.org/pub/gnu/emacs/emacs-27.1.tar.gz \
        && tar -xzf emacs-27.1.tar.gz \
        && cd emacs-27.1 \
        && JSON_CFLAGS='-I/usr/include' JSON_LIBS='-ljansson' \
           LIBGNUTLS_CFLAGS='-I/usr/include' LIBGNUTLS_LIBS='-lgnutls' \
           LIBXML2_CFLAGS='-I/usr/include/libxml2' LIBXML2_LIBS='-lxml2' \
        ./configure --without-makeinfo --with-modules \
        && make install \
        && apt-get remove -y gcc perl \
        && apt-get autoremove -y \
        && rm -rf /emacs-27.1.tar.gz /emacs-27.1 /var/lib/apt/lists/* /root/.cache/*

ENV WORKSPACE "/workspace"
ENV ELISP_PATH "${WORKSPACE}/pkg"

RUN useradd emacser -d $WORKSPACE \
        && mkdir -p $ELISP_PATH \
        && chown -R emacser $WORKSPACE
USER emacser:emacser

COPY docker/requirements.el $WORKSPACE
RUN emacs --script $WORKSPACE/requirements.el

COPY _requirements.el $WORKSPACE
RUN emacs --script $WORKSPACE/_requirements.el

COPY --chown=emacser:emacser docker/.emacs $WORKSPACE
COPY --chown=emacser:emacser pkg $ELISP_PATH
COPY --chown=emacser:emacser melpazoid/melpazoid.el $ELISP_PATH

ARG PACKAGE_MAIN
ENV PACKAGE_MAIN "${PACKAGE_MAIN}"

WORKDIR $ELISP_PATH
CMD ["emacs", "--script", "melpazoid.el"]
