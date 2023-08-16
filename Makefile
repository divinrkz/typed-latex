run:
	dune exec typed_latex

test:
	dune test

build:
	dune build

error:
	menhir --list-errors lib/parser.mly > lib/parser.messages

.PHONY: run build test error
