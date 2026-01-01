#!/bin/bash
# melpazoid-local.sh
case "${1}" in
--help|-h|-?|--usage)
   printf "\n$0 [path-to-repo-root [recipe-filename]]\n
Helper script for running melpazoid locally.\n
What that means: You have docker installed, probably running 'rootless',
and you want to perform the melpazoid lint-checking routine on a local
elisp package repository for which you have a melpa 'recipe' written.
If you haven't previously downloaded the melpazoid docker image, this
process will need to do that (about 700 MB disk space).\n\n"
   exit
   ;;
esac

if [ -d ./melpazoid ] ; then
  MELPAZOID_PATH=./melpazoid
elif [ -d ${HOME}/.local/lib/melpazoid ] ; then
  MELPAZOID_PATH=${HOME}/.local/lib/melpazoid
elif [ -d /usr/local/lib/melpazoid ] ; then
  MELPAZOID_PATH=/usr/local/lib/melpazoid
else
  echo "$0: Error. Melpazoid directory not found. Exiting ..."
  exit 1
fi

LOCAL_REPO="${1}"
RECIPE_PATH="${2}"

SERVICE_MANAGER=$(ps -p 1 -o comm=)
case ${SERVICE_MANAGER} in
systemd)
  if ! systemctl is-active --user --quiet docker.service; then
    echo "Docker service (--user) is not running."
    exit
  fi
  ;;
init)
  if ! service docker status > /dev/null 2>&1; then
    echo "Docker service is not running."
    exit
  fi
  ;;
esac

LOCAL_REPO="${LOCAL_REPO/#\~/$HOME}"
while [ ! -d "${LOCAL_REPO}" ] ; do
  read -ep "Enter path to local repository root: " LOCAL_REPO
  LOCAL_REPO="${LOCAL_REPO/#\~/$HOME}"
done
if [[ "${LOCAL_REPO: -1}" != "/" ]]; then
  LOCAL_REPO="${LOCAL_REPO}/"
fi
export LOCAL_REPO="${LOCAL_REPO}"

while [ ! -f "${LOCAL_REPO}${RECIPE_PATH}" ] ; do
  read -ep "Enter package recipe filename (relative to root): " RECIPE_PATH
  done
export RECIPE=$( < "${LOCAL_REPO}${RECIPE_PATH}" )

if ! docker images |grep "^melpazoid" &>/dev/null ; then
  printf "ATTENTION: No local melpazoid image found !\n\n
That means the melpazoid program will have docker build one.
Expect this to take some time, but the image should persist
locally, so subsequent runs will execute much quicker.\n\n"
fi

OUT_FILE="${LOCAL_REPO}melpazoid_output_$( printf "%(%F_%H%M%S)T" ).txt"
printf "Running.\nOutput is being re-directed to:\n${OUT_FILE}\n..."
echo "${OUT_FILE}\n" > "${OUT_FILE}"
exec >>"${OUT_FILE}" 2>&1
cd "$MELPAZOID_PATH"
make
rm -rf pkg/*
exec > /dev/tty 2>/dev/tty
printf "\ndone!\n\nPress Ctrl-C to exit,
or press any other key to view the output using 'less'.\n\n"
less "${OUT_FILE}"
cd - &>/dev/null
