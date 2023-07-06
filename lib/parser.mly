(*
  All tokens capture their start position, along with any other potential data.
  This data can be extracted by the parser using `fst` and `snd`.

  Also, there are a ton of shift/reduce errors, but those can (and probably won't be) fixed later (see: "seems to work")
*)

%{
  (* fix a weird menhir issue: https://github.com/ocaml/dune/issues/2450 *)
  module Typed_latex = struct end

  (* TODO: better error messages *)
  (* see https://www.baturin.org/blog/declarative-parse-error-reporting-with-menhir/ *)
%}

(* general tokens *)
%token <Lexing.position> LINE_BREAK
%token <Lexing.position> WHITESPACE
%token <Lexing.position * string> LINE_COMMENT
%token <Lexing.position> LEFT_CURLY
%token <Lexing.position> LEFT_BRACKET
%token <Lexing.position> LEFT_PAREN
%token <Lexing.position> RIGHT_CURLY
%token <Lexing.position> RIGHT_BRACKET
%token <Lexing.position> RIGHT_PAREN
%token <Lexing.position> COMMA
%token <Lexing.position> AMPERSAND
%token <Lexing.position> PIPE
%token <Lexing.position * string> WORD
%token <Lexing.position * string> COMMAND
%token <Lexing.position> EOF

(* latex tokens *)
%token <Lexing.position * string> BEGIN
%token <Lexing.position * string> END
%token <Lexing.position * string> MATHMODE

(* math tokens *)
%token <Lexing.position> EQ
%token <Lexing.position> PLUS
%token <Lexing.position> MINUS
%token <Lexing.position> TIMES
%token <Lexing.position> CARET
%token <Lexing.position> UNDERSCORE
%token <Lexing.position> FRAC
%token <Lexing.position> SEPARATOR
%token <Lexing.position * int> INTEGER
%token <Lexing.position * string> VARIABLE
%token <Lexing.position> LE
%token <Lexing.position> GE
%token <Lexing.position> LEQ
%token <Lexing.position> GEQ
%token <Lexing.position> SET_IN
%token <Lexing.position> SET_OPEN
%token <Lexing.position> SET_CLOSE
%token <Lexing.position> SET_NOTIN
%token <Lexing.position> SET_UNION
%token <Lexing.position> SET_INTER
%token <Lexing.position> SUBSET
%token <Lexing.position> SUPERSET
%token <Lexing.position> SUBSETEQ
%token <Lexing.position> SUPERSETEQ
%token <Lexing.position> IMPLIES
%token <Lexing.position> IFF
%token <Lexing.position> FORALL
%token <Lexing.position> EXISTS
%token <Lexing.position> SUCHTHAT
%token <Lexing.position> LAND
%token <Lexing.position> LOR
%token <Lexing.position> LNOT

%start <Ast.Latex.t list option> start
%start <Ast.Math.t option> math_mode

%type <Ast.Text.word> word
%type <Ast.Latex.t> content
%type <Ast.Environment.t> environment
%type <string> curly_word
%%

start:
| all = content+; EOF { Some all }
| EOF { None }

content:
| env = environment { {Ast.Node.pos = Ast.loc_zero; value = Ast.Latex.Environment env} }
 (* below leads to a shift/reduce conflict, but menhir seems to work fine anyway *)
| text = word+ { {Ast.Node.pos = Ast.loc_zero; value = Ast.Latex.Text text} } 
| math = MATHMODE { {Ast.Node.pos = Ast.loc_zero; value = Ast.Latex.Mathmode (snd math)} } 

word:
| word_data = WORD { Ast.Text.Word (snd word_data) }
| LINE_BREAK { Ast.Text.Linebreak }
| WHITESPACE { Ast.Text.Whitespace }
| LINE_COMMENT { Ast.Text.Whitespace }
| COMMA { Ast.Text.Comma }
| PIPE { Ast.Text.Pipe }

(* TODO: environment args *)
environment:
| name1 = BEGIN; body = content+; name2 = END; { (snd name1, snd name2), body }

curly_word:
| LEFT_CURLY; word_data = WORD; RIGHT_CURLY { (snd word_data) }

(* TODO: figure out a better way of dealing with whitespace *)
(* for now, I'm just doing whitespace right-recursively to appease the parser *)
math_mode:
| WHITESPACE?; relation = relation1; EOF { Some relation }
| WHITESPACE?; statement = statement; EOF { Some statement }
| EOF { None }


(* just specify precedence as part of the grammar bc idk exactly how menhir precedence annotations work *)

statement:
(* forall X, Y *)
| FORALL; WHITESPACE?; rel = relation1; COMMA; WHITESPACE?; next = relation1; { Ast.Math.Forall (rel, next) }
| FORALL; WHITESPACE?; rel = relation1; COMMA; WHITESPACE?; next = statement; { Ast.Math.Forall (rel, next) }
(* exists X : Y *)
| EXISTS; WHITESPACE?; rel = relation1; SUCHTHAT; WHITESPACE?; next = relation1; { Ast.Math.Exists (rel, Ast.Math.Suchthat next) }
| EXISTS; WHITESPACE?; rel = relation1; SUCHTHAT; WHITESPACE?; next = statement; { Ast.Math.Exists (rel, Ast.Math.Suchthat next) }

(* TODO: technically i think these shouldn't have the same precedence? *)
relation1:
| lhs = relation1; LAND; WHITESPACE?; rhs = relation2; WHITESPACE?; { Ast.Math.Op (lhs, Ast.Math.And, rhs) }
| lhs = relation1; LOR; WHITESPACE?; rhs = relation2; WHITESPACE?; { Ast.Math.Op (lhs, Ast.Math.Or, rhs) }
| rel = relation2 { rel }

relation2:
| lhs = relation2; rel = rel; rhs = expr; WHITESPACE?; { Ast.Math.Rel (lhs, rel, rhs) }
(* grouping *)
| LEFT_PAREN; WHITESPACE?; rel = relation1; RIGHT_PAREN; WHITESPACE?; { Ast.Math.Grouping rel }
| LEFT_CURLY; WHITESPACE?; rel = relation1; RIGHT_CURLY; WHITESPACE?; { Ast.Math.Grouping rel }
| LEFT_BRACKET; WHITESPACE?; rel = relation1; RIGHT_BRACKET; WHITESPACE?; { Ast.Math.Grouping rel }
(* next case *)
| expr = expr; { expr }

(* precedence (high to low):
  - literals, implicit multiplication, function application
  - times
  - plus, minus
  - set union, set intersection
  Relations:
  - set_in, set_notin
  - subset, ssubset, superset, ssuperset
  - implies, iff
  Statements:
  - forall, exists, such that
  *)

expr:
| infix = infix1; { infix }

infix1:
| lhs = infix1; SET_INTER; WHITESPACE?; rhs = infix2; WHITESPACE?; { Ast.Math.Op (lhs, Ast.Math.Inter, rhs) }
| lhs = infix1; SET_UNION; WHITESPACE?; rhs = infix2; WHITESPACE?; { Ast.Math.Op (lhs, Ast.Math.Union, rhs) }
| infix = infix2; { infix }

infix2:
| lhs = infix2; PLUS; WHITESPACE?; rhs = infix3; WHITESPACE?; { Ast.Math.Op (lhs, Ast.Math.Plus, rhs) }
| lhs = infix2; MINUS; WHITESPACE?; rhs = infix3; WHITESPACE?; { Ast.Math.Op (lhs, Ast.Math.Minus, rhs) }
| infix = infix3; { infix }

infix3:
| lhs = infix3; TIMES; WHITESPACE?; rhs = literal; WHITESPACE?; { Ast.Math.Op (lhs, Ast.Math.Times, rhs) }
| infix = infix4; { infix }

infix4:
| lhs = infix4; UNDERSCORE; rhs = literal; WHITESPACE?; { Ast.Math.Subscript (lhs, rhs) }
| lhs = infix4; CARET; rhs = literal; WHITESPACE?; { Ast.Math.Superscript (lhs, rhs) }
| unary = unary; { unary }

unary:
| MINUS; rhs = literal; { Ast.Math.Unary (Ast.Math.Negate, rhs) }
| LNOT; WHITESPACE?; rhs = literal; { Ast.Math.Unary (Ast.Math.Not, rhs) }
| rhs = literal { rhs }

literal:
(* allow interpreting concatenation as multiplication *)
| lhs = INTEGER; WHITESPACE?; rhs = literal; { Ast.Math.Op (Ast.Math.Literal (snd lhs), Ast.Math.Times, rhs) }
| lhs = VARIABLE; WHITESPACE?; rhs = literal; { Ast.Math.Op (Ast.Math.Variable (snd lhs), Ast.Math.Times, rhs) }
| num = INTEGER; WHITESPACE?; { Ast.Math.Literal (snd num) }
| var = VARIABLE; WHITESPACE?; { Ast.Math.Variable (snd var) }
| f = function_call; { f }
(* set literal *)
(* either a list of expressions or a comprehension *)
(* fraction *)
| FRAC; LEFT_CURLY; WHITESPACE?; numerator = expr; RIGHT_CURLY; LEFT_CURLY; WHITESPACE?; denominator = expr; RIGHT_CURLY; WHITESPACE?; { Ast.Math.Op (numerator, Ast.Math.Frac, denominator) }
(* command with arg *)
| command = COMMAND; LEFT_CURLY; arg = expr; RIGHT_CURLY; WHITESPACE? { Ast.Math.Command ((snd command), Some arg)}
(* command without args *)
| command = COMMAND; WHITESPACE? { Ast.Math.Command ((snd command), None)}
(* loop back to expr if grouping spotted *)
| LEFT_PAREN; WHITESPACE?; expr = expr; RIGHT_PAREN; WHITESPACE?; { Ast.Math.Grouping expr }
| LEFT_CURLY; WHITESPACE?; expr = expr; RIGHT_CURLY; WHITESPACE?; { Ast.Math.Grouping expr }
| LEFT_BRACKET; WHITESPACE?; expr = expr; RIGHT_BRACKET; WHITESPACE?; { Ast.Math.Grouping expr }

function_call:
| lhs = VARIABLE; LEFT_PAREN; rhs = separated_list(comma_sep, expr); RIGHT_PAREN; WHITESPACE?; { Ast.Math.Apply (Ast.Math.Variable (snd lhs), rhs) }

comma_sep:
| COMMA; WHITESPACE?; {}

%inline rel:
| LE; WHITESPACE?; { Ast.Math.Le }
| GE; WHITESPACE?; { Ast.Math.Ge }
| LEQ; WHITESPACE?; { Ast.Math.Leq }
| GEQ; WHITESPACE?; { Ast.Math.Geq }
| EQ; WHITESPACE?; { Ast.Math.Eq }
| SUBSET; WHITESPACE?; { Ast.Math.Subset }
| SUBSETEQ; WHITESPACE?; { Ast.Math.SubsetEq }
| SUPERSET; WHITESPACE?; { Ast.Math.Superset }
| SUPERSETEQ; WHITESPACE?; { Ast.Math.SupersetEq }
| SET_IN; WHITESPACE?; { Ast.Math.In }
| SET_NOTIN; WHITESPACE?; { Ast.Math.NotIn }
| IMPLIES; WHITESPACE?; { Ast.Math.Implies }
| IFF; WHITESPACE?; { Ast.Math.Iff }
