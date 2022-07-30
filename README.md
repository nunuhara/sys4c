sys4c
=====

This is an experimental compiler for AliceSoft's System 4 programming language,
written in OCaml. The goal of this project is to eventually replace the
somewhat more mature compiler in alice-tools (written in C).

Building
--------

The following dependencies should be installed either via your system's
package manager or manually:

* make
* meson
* libpng
* libturbojpeg
* libwebp
* zlib

e.g. on debian,

    # apt install make meson libpng-dev libturbojpeg0-dev libwebp-dev zlib1g-dev

The following dependencies should be installed via opam:

* ocaml
* core
* ctypes-foreign
* dune
* menhir

    $ opam install core ctypes-foreign dune menhir

Then fetch the git submodules,

    git submodule init
    git submodule update

(Alternatively, pass `--recurse-submodules` when cloning this repository.)

Then build the compiler with make,

    make

Installation
------------

Run the command `dune install` to install sys4c (via opam). This does not
require root.

Alternatively, you can run the compiler without installing it with
`dune exec src/sys4c.exe`.

Usage
-----

### Batch Mode

To compile one or more .jaf source files in batch mode, simply pass them to the
compiler on the command line. The `-o` option is used to specify the output
.ain file name (defaults to `out.ain`).

    sys4c -o foobar.ain foo.jaf bar.jaf

### Separate Compilation

To compile a source file in unit mode, the `-c` option is used. If the unit
depends on definitions from another source file, that source file should be
compiled first and passed on the command line using the `-i` option:

    sys4c -c -o foo.ain foo.jaf
    sys4c -c -o bar.ain -i foo.ain bar.jaf

Once all of the source files have been compiled as units, they can be linked
to create the final .ain file similar to how .jaf files are compiled in batch
mode:

    sys4c -o foobar.ain foo.ain bar.ain

At this point, a source file needs to be recompiled if:
  1. The source file was modified, or
  2. Any of the external definitions it depends on have changed.

In order to determine (2), the old .ain file can be compared against its
dependencies using the -m option:

    sys4c -m foo.ain bar.ain

If this returns non-zero, the source file should be recompiled.
In bash, you can check if a source file needs to be recompiled as follows:

    if [ ! -f bar.ain ] || [ bar.jaf -nt bar.ain ] || ! sys4c -m foo.ain bar.ain; then
        sys4c -c -i foo.ain -o bar.ain bar.jaf
    fi

An example makefile-based build system is provided in the
`extras/project-template` directory in this repository.
