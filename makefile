.PHONY: default build install uninstall clean

default: build

build:
	./build-libsys4.sh
	dune build

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

clean:
	ninja -C libsys4/libsys4/build clean
	dune clean

