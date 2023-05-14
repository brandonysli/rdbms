.PHONY: test check testparse

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	-OCAMLRUNPARAM=b dune exec test/main.exe 
	-OCAMLRUNPARAM=b dune exec testparse/main.exe

doc:
	dune build @doc

clean:
	dune clean
	rm -f rml.zip

opendoc: doc
	@bash opendoc.sh

print:
	OCAMLRUNPARAM=b dune exec demo/ms2.exe

testparse:
	OCAMLRUNPARAM=b dune exec testparse/main.exe