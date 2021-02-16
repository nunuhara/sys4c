sys4c
=====

This is an experimental compiler for AliceSoft's System 4 programming language,
written in OCaml. The goal of this project is to eventually replace the
somewhat more mature compiler in alice-tools (written in C).

Building
--------

With *OCaml* and the *dune* build system installed, run:

    dune build src/sys4c.exe

Usage
-----

Not yet usable. But to run it anyways,

    dune exec src/sys4c.exe

Then enter some code followed by the '$' character. Currently this just prints
the code back (from the parsed AST).

TODO
----

- [x] Parser
- [ ] Symbol table
- [ ] Type analysis
- [ ] Constant expression evaluation
- [ ] Variable allocation
- [ ] Compilation
- [ ] Separate compilation & linking (maybe?)
