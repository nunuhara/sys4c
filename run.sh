#!/bin/sh

dune exec -- src/sys4c.exe "$@"
alice ain dump -c out.ain
