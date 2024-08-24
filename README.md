# typed_latex

`typed_latex` is a tool that analyzes LaTeX written math proofs and parses natural language to identify common proof errors such as, usage of undefined variables, incorrect variable quantification, variable type mismatch, misuse of implications and equivalences, unnecessary re-definitions, unjustified assumptions, lack of proper citations for definitions, and missing conclusions; as well as providing real time feedback for each of the errors. 

## Features
**1. Type Checking**: Ensures that mathematical proofs written in LaTeX are valid. <br>
**2. Error Reporting**: Highlights errors and provides suggestions for corrections. <br>
**3. Seamless Integration**: Easily integrates with your existing LaTeX workflow.


## Installation
To install `typed_latex`, follow these steps:

#### Prerequisites
Ensure you have `opam` and `dune` installed on your system.

#### Steps
1. Clone the repository:
```sh
git clone https://gitlab.caltech.edu/blank-lab/typed_latex
```

2. Create and switch to a new Opam switch:
```sh
opam switch create typed_latex 5.1.1
eval $(opam env)
```

3. Install dependencies: <br>
Inside the project directory containing your dune-project file, run:
```sh
opam install . --deps-only
```

4. Build the project:
```sh
make build 
```
5. Run the project:
```sh
make run
```

## Project Structure
__This project is currently unfinished__

### Overview
The first step in the overall workflow is to parse the LaTeX file. A previous version of this project had
an attempt at writing a LaTeX parser from scratch in Ocaml / Menhir. However, this parser was incomplete
and would fail to parse some valid latex files. The complexity of the LaTeX language made fixing and extending
this parser a difficult and largely pointless task, so instead we looked to replace it with an existing LaTeX
parser. We found no existing complete LaTeX parsers written in Ocaml. Instead, we chose to use two Python
LaTeX parsers: one main one and one for math expressions. The main Python LaTeX parser TexSoup allows
convenient parsing. However, it does not parse LaTeX math expressions. To parse math, we use latex2sympy
(specifically, ANTLR, which is a port of latex2sympy built into sympy). This conveniently translates a LaTeX
math expression directly into a Sympy expression (Sympy is a Python symbolic math library). Sympy kindly
provides a `srepr` function to export a Sympy object as a conveniently-formatted string (The string is
almost purely just a tree of function calls, and therefore easier to parse). Then, our Python-side serializes
this parsed LaTeX into a JSON file, which is then passed to the Ocaml side of the program. On the Ocaml-side,
we start by deserializing from the JSON file (`latex_deserializer.ml`) into a tree-like AST
(`RawLatex` / `RawMathLatex`). This AST is lexed (`proof_lex.ml`) into several semi-flat sequences of tokens
(`ProofToken`). These token streams can now be passed to the pattern-matching system (`patterns.ml`).

The core of the pattern-matching system is a regular-expression–like type (`pattern / math_pattern`) that
allows you to express general patterns in the structure of a token stream. Using `match_pattern`, you can
match for patterns on a token stream. `match_pattern` outputs a `context`, which includes the region matched
and a container (`MatchContainer`) that contains info about the match; specifically: any sub-MatchContainers
(captured by the DefContainer pattern), any natural-language type names (captured by the TypeName pattern),
any general math expressions (captured by the Expression math pattern), any function names (captured by the
Function math pattern), and any leaf math symbols (captured by the TerminalSymbol math pattern). Each one of
these capturing patterns and math patterns is parametrized by an id (`MatchID`). This id can then be used to
identify the matched values in the `MatchContainer`. More interesting than just the `match_pattern` function,
we have the `replace_pattern` and `exhaustively_replace_all` functions that allow for "find-and-replace"-like
behavior in our token streams. While the patterns could all be directly hardcoded in Ocaml, it was more
convenient to make a list of text regex-like patterns (in `assets/patterns`) that are then extracted and
translated into our `pattern` type (`pattern_extractor.ml`).

Next, we have our typing system (`typing.ml`). When designing the typing system, there were a few key considerations and
needed capabilities:
- We need type inheritance and polymorphism (so we can, for instance, treat the `Nat` 3 as not only a `Nat` type
but also an `Int`, `Rational`, `Real`, etc.).
- We need generic type constructors (for instance, a List).
- We need type variables (type -> type rules) (so we can, for instance, distinguish between a list of Ints
and a list of lists of `Int`s).
- We need covariance and contravariance (so we can, for instance, pass a list of Nats to a function accepting
lists of `Real`s, or a `Int -> Int` function to a function requiring a `Nat -> Int` function).
- We need polymorphic type parameters (type -> value rules) (see https://en.wikipedia.org/wiki/System_F) (so we can reason about the
types of unknown-type values or values parametrized over a type (for instance, the `id : a -> a` function, which must return an `Int` when given an `Int`, or a List of `Nat`s when given a list of `Nat`s. For another example, we may want a `head : List a -> a` function)).
- We need union types (so we can, for instance, write the type of the `(+) : Nat -> Nat -> Nat | Int -> Int -> Int | Rational -> Rational -> Rational | ...` function: we need to be able to use the sum of two `Nat`s as a `Nat` but the sum of two `Real`s isn't a `Nat`).
