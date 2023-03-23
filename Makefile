.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

print:
	OCAMLRUNPARAM=b dune exec test/main.exe