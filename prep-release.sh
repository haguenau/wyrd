#!/bin/bash
# 'prep-release' script
# Make all the last-minute changes to prepare the sources for packaging
# in a release tarball
#
# Usage: prep-release.sh DESTDIR
#

set -e

CURSES_BRANCH=lp:ubuntu/ocaml-curses
CURSES_REVISION=8

echo "Exporting revision..."
bzr export $1
echo "Exporting dependencies..."
bzr export -r $CURSES_REVISION $1/curses $CURSES_BRANCH

cd $1
pushd curses
echo "Generating curses/configure ..."
autoheader && autoconf && rm -rf autom4te.cache
popd

echo "Generating ./configure ..."
autoconf && rm -rf autom4te.cache
echo "Generating _oasis and setup.ml ..."
./make_oasis.ml && oasis setup
echo "Creating documentation..."
cd doc && make &> /dev/null
echo "Done."

