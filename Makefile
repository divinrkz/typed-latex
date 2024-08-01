repl:
	dune utop .

run:
	dune exec typed_latex

test:
	dune test

build:
	dune build

# error:
# 	menhir --list-errors lib/latex_parser.mly > lib/latex_parser.messages
# 	menhir --list-errors lib/math_parser.mly > lib/math_parser.messages

.PHONY: run build test# error
