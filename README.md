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
