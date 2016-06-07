#!/bin/sh

oops() {
  echo "$@" >&2
  exit 1
}

set -x
while true ; do

  master=`git branch -v | grep -w master | tr -d '*' | awk '{print $2}'`
  release=`git branch -v | grep -w release | tr -d '*' | awk '{print $2}'`

  if test "$release" != "$master" ; then
    git merge --no-ff master || oops "merge failed"
    cabal build
  fi

  sleep 5

done
