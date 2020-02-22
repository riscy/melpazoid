#!/bin/bash

for filename in $ELISP_PATH/*.el; do
    MELPAZOID_FILENAME=$filename /usr/bin/emacs --script melpazoid.el
done
