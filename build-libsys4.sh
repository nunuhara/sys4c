#!/bin/sh

chdir $(dirname "$0")/src/libsys4

if [ ! -d build ]
then
    mkdir build
fi

if [ ! -e build/build.ninja ]
then
    meson build
fi

ninja -C build
