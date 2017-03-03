#!/bin/bash

set -e

CURSES_BRANCH=lp:ubuntu/ocaml-curses
CURSES_REVISION=8

echo "Getting curses lib ..."
bzr branch -r $CURSES_REVISION $CURSES_BRANCH curses

pushd curses
echo "Generating curses/configure ..."
autoheader && autoconf && rm -rf autom4te.cache
popd

echo "Generating ./configure ..."
autoconf && rm -rf autom4te.cache


