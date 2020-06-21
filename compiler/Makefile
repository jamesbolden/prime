.ALWAYS: repin

build:
	dune external-lib-deps --missing @install
	dune build @install

test:
	dune build @runtest

run: build
	dune exec pc

pin:
	opam pin add .

repin: build
	opam upgrade pc

build-all:
	dune build @install

clean:
	dune clean

.PHONY: build test pin repin build-all