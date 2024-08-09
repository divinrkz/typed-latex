# typed_latex

`typed_latex` is a tool that analyzes LaTeX written math proofs and parses natural language to identify common proof errors such as, usage of undefined variables, incorrect variable quantification, variable type mismatch, misuse of implications and equivalences, unnecessary re-definitions, unjustified assumptions, lack of proper citations for definitions, and missing conclusions; as well as providing real time feedback for each of the errors. 

## Features
**1. Type Checking**: Ensures that mathematical proofs written in LaTeX are valid. <br>
**2. Error Reporting**: Highlights errors and provides suggestions for corrections. <br>
**3. Seamless Integration**: Easily integrates with your existing LaTeX workflow.


## Installation
To install `typed_latex`, follow these steps:

#### Prerequisites
Ensure you have `opam`, `dune`, and `python` installed on your system.

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

4. Run the Python side of the transformer:
```sh
python transformer tex_file_name.tex
```

5. Build the Ocaml side of the project:
```sh
make build 
```
6. Run the Ocaml side of the project:
```sh
make run
```

## Project Structure
__This project is currently unfinished__

### Overview
The first step in the overall workflow is to parse the LaTeX file. A previous version of this project had an attempt at writing a LaTeX parser from scratch in Ocaml / Menhir. However, this parser was incomplete and would fail to parse some valid latex files. The complexity of the LaTeX language made fixing and extending this parser a difficult and largely pointless task, so instead we looked to replace it with an existing LaTeX parser. We found no existing complete LaTeX parsers written in Ocaml. Instead, we chose to use two Python LaTeX parsers: one main one and one for math expressions. The main Python LaTeX parser TexSoup allows convenient parsing. However, it does not parse LaTeX math expressions. To parse math, we use latex2sympy (specifically, ANTLR, which is a port of latex2sympy built into sympy). This conveniently translates a LaTeX math expression directly into a Sympy expression (Sympy is a Python symbolic math library). Sympy kindly provides a `srepr` function to export a Sympy object as a conveniently-formatted string (The string is almost purely just a tree of function calls, and therefore easier to parse). Then, our Python-side serializes this parsed LaTeX into a JSON file, which is then passed to the Ocaml side of the program. On the Ocaml-side, we start by deserializing from the JSON file (`latex_deserializer.ml`) into a tree-like AST (`RawLatex` / `RawMathLatex`). This AST is lexed (`proof_lex.ml`) into several semi-flat sequences of tokens (`ProofToken`). These token streams can now be passed to the pattern-matching system (`patterns.ml`).

The core of the pattern-matching system is a regular-expression–like type (`pattern / math_pattern`) that allows you to express general patterns in the structure of a token stream. Using `match_pattern`, you can match for patterns on a token stream. `match_pattern` outputs a `context`, which includes the region matched and a container (`MatchContainer`) that contains info about the match; specifically: any sub-MatchContainers (captured by the DefContainer pattern), any natural-language type names (captured by the TypeName pattern), any general math expressions (captured by the Expression math pattern), any function names (captured by the Function math pattern), and any leaf math symbols (captured by the TerminalSymbol math pattern). Each one of these capturing patterns and math patterns is parametrized by an id (`MatchID`). This id can then be used to identify the matched values in the `MatchContainer`. More interesting than just the `match_pattern` function, we have the `replace_pattern` and `exhaustively_replace_all` functions that allow for "find-and-replace"-like behavior in our token streams. While the patterns could all be directly hardcoded in Ocaml, it was more convenient to make a list of text regex-like patterns (in `assets/patterns`) that are then extracted and translated into our `pattern` type (`pattern_extractor.ml`).

Next, we have our typing system (`typing.ml`). When designing the typing system, there were a few key considerations and
needed capabilities:
- We need type inheritance and polymorphism (so we can, for instance, treat the `Nat` 3 as not only a `Nat` type
but also an `Int`, `Rational`, `Real`, etc.).
- We need generic type constructors (for instance, a List).
- We need type variables (type -> type rules) (so we can, for instance, distinguish between a list of Ints
and a list of lists of `Int`s).
- We need covariance and contravariance (so we can, for instance, pass a list of Nats to a function accepting
lists of `Real`s, or a `Int -> Int` function to a function requiring a `Nat -> Int` function).
- We need polymorphic type parameters (type -> value rules) (see [https://en.wikipedia.org/wiki/System_F](https://en.wikipedia.org/wiki/System_F)) (so we can reason about the
types of unknown-type values or values parametrized over a type (for instance, the `id : a -> a` function, which must return an `Int` when given an `Int`, or a List of `Nat`s when given a list of `Nat`s. For another example, we may want a `head : List a -> a` function)).
- We need union types (so we can, for instance, write the type of the `(+) : Nat -> Nat -> Nat | Int -> Int -> Int | Rational -> Rational -> Rational | ...` function: we need to be able to use the sum of two `Nat`s as a `Nat` but the sum of two `Real`s isn't a `Nat`).

Putting this together, we get a few a few key components. First, we have a `TypeConstructor`, which is either a `Top` type constructor ([supertype of everything](https://en.wikipedia.org/wiki/Top_type)), a `Bottom` type constructor ([subtype of everything](https://en.wikipedia.org/wiki/Bottom_type)), or a `TC` parametrized by the name, a list of type dependency variances (`Variance`) (doubling as the number of type dependencies), and a set of supertypes. You don't need to include `Top` as a supertype — it is automatically so. Supertypes are also made to automatically propagate, so if `Int` is in the supertypes of `Nat` and `Rational` is in the supertypes of `Int`, then `Rational` is automatically a supertype of `Nat`. A few example type constructors are written in the `BuiltIn` sub-sub-module. Please keep in mind, though, the distinction between `TypeConstructor`s and `Types`. `Top` or `int_c`, for instance, are type constructors, not types. Next, we have 3 types of `Type`: `Union` types, `FTypes` (polymorphic types), and `Concrete` types. `Concrete` types are parametrize by the type from the co-recursive `ConcreteType` module. A `ConcreteType` is parametrized by the type constructor and list of type-parameters. Unfortunately, Ocaml's type system is not strong enough to ensure that the number of given type-parameters matches the number of variance descriptors in the associated type constructor 🙁. Therefore, the `ConcreteType.t` type is made opaque so that it cannot be incorrectly constructed. Instead, the `ConcreteType` module includes a constr method to create a `ConcreteType.t` and validate the number of type-parameters at run-time. Since you can't pattern match on the opaque type of `ConcreteType`, `ConcreteType` also includes some getter and helper methods. The `Type` module includes functions for assisting with operations on types, such as subtype and supertype checking. Finally, we have types for typed values (`Value`) and named typed values (`Symbol`). 

### Project structure
- `typed_latex/`
    - `assets/`
        - `json/`
            - `parsed-latex.json`\
            The intermediate transformer representation, created by the Python side and read by the Ocaml side.
        - `patterns/`\
        Pattern definition files to be extracted by `pattern_extractor.ml`.
        - `tex/`\
        Sample LaTeX files. The Python side of the transformer will read from this directory.
    - `bin/`
        - `dune`\
        The bin dune file.
        - `main.ml`\
        Main program file.
    - `lib/`
        - `comparable_extension.ml`\
        Extension functor to modules derived from the `Core.Comparable` functor including some infix operators and helpful functions.
        - `construction.ml`\
        A constructive representation of data (very incomplete).
        - `dune`\
        The lib dune file.
        - `latex_aux.ml`\
        Helper functions for working with `RawLatex`.
        - `latex_deserializer.ml`\
        `RawLatex` and `RawMathLatex` definitions, helper functions, and parsing.
        - `pattern_extractor.ml`\
        Extract `pattern`s from pattern description files.
        - `patterns.ml`\
        `pattern`, `MatchID`, and `MatchContainer` definitions, and pattern matching and find-and-replace functionality.
        - `proof_lex.ml`\
        Flatten and decompose `RawLatex` into a token stream.
        - `string_tree.ml`\
        A string tree a uniform representation type for displaying datastructures.
        - `typing.ml`\
        Type definitions and operations.
        - `util.ml`\
        Various utility functions and values.
    - `transformer/`\
        The Python side of the transformer.
        - `__main__.py`\
        The main transformer file.
        - `notations.py`\
        Extra symbol definition list.
        - `utils.py`\
        Utility functions and values.
    - `dune-project`\
    Dune project branding.
    - `Makefile`\
    Project build and run configuration (calls dune build and run).
    - `typed_latex.opam`\
    Main dune project configuration.
