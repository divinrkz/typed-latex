run: parse_latex run_only

run_only:
	dune exec typed_latex

test:
	dune test

build:
	dune build

parse_latex:
	./node_modules/latex-utensils/bin/luparse tex/sample.tex > parsed_latex.json

install:
	npm install latex-utensils

# error:
# 	menhir --list-errors lib/latex_parser.mly > lib/latex_parser.messages
# 	menhir --list-errors lib/math_parser.mly > lib/math_parser.messages

.PHONY: run build test# error
