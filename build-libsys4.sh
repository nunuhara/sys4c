#!/bin/sh

chdir $(dirname "$0")/libsys4/libsys4

# files in subdirectories aren't available when building c stubs
# so we need to copy and flatten the include directory structure
sed 's/#include "system4\//#include "system4_/g' include/system4.h > ../system4.h
sed 's/#include "system4\//#include "system4_/g' include/system4/ain.h > ../system4_ain.h
sed 's/#include "system4\//#include "system4_/g' include/system4/buffer.h > ../system4_buffer.h
sed 's/#include "system4\//#include "system4_/g' include/system4/instructions.h > ../system4_instructions.h
sed 's/#include "system4\//#include "system4_/g' include/system4/string.h > ../system4_string.h
sed 's/#include "system4\//#include "system4_/g' include/system4/utfsjis.h > ../system4_utfsjis.h

if [ ! -d build ]
then
    mkdir build
fi

if [ ! -e build/build.ninja ]
then
    meson build -Ddefault_library=both
fi

ninja -C build
