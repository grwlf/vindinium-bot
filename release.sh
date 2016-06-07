#!/bin/sh

oops() {
  echo "$@" >&2
  exit 1
}

set -x
while true ; do

  git fetch

  if git branch -v | grep -q behind ; then
    git pull || oops "Pull failed"
    cabal build
  fi

  sleep 5

done
