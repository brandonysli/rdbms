.PHONY: test check testparse cloc

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
	rm -f final.zip

opendoc: doc
	@bash opendoc.sh

print:
	OCAMLRUNPARAM=b dune exec demo/ms2.exe

testparse:
	OCAMLRUNPARAM=b dune exec testparse/main.exe

repl:
	OCAMLRUNPARAM=b dune exec bin/repl.exe

cloc:
	-dune clean
	cloc --by-file --include-lang=OCaml .

check: 
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f final.zip
	zip -r final.zip . -x@exclude.lst