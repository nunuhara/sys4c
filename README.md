sys4c
=====

This is an experimental compiler for AliceSoft's System 4 programming language,
written in OCaml. The goal of this project is to eventually replace the
somewhat more mature compiler in alice-tools (written in C).

Building
--------

First install the dependencies:

* OCaml
* dune
* menhir
* make
* meson
* libpng
* libturbojpeg
* libwebp
* zlib

Then fetch the git submodules,

    git submodule init
    git submodule update

(Alternatively, pass `--recurse-submodules` when cloning this repository.)

Then build the compiler with make,

    make

Usage
-----

Not yet usable. But to run it anyways,

    dune exec src/sys4c.exe

Then enter some code followed by the '$' character. Currently this just does
some analysis and then prints the code back (from the parsed AST).

TODO
----

- [x] Parser
- [x] Toplevel declaration analysis
- [x] Type analysis
- [x] Constant expression evaluation
- [x] Variable allocation
- [ ] Compilation
- [ ] Separate compilation & linking (maybe?)
