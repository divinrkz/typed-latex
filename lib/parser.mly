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
%token <Lexing.position * string> MULTILINE

(* math tokens *)
%token <Lexing.position> EQ
%token <Lexing.position> PLUS
%token <Lexing.position> MINUS
%token <Lexing.position> TIMES
%token <Lexing.position> CARET
%token <Lexing.position> UNDERSCORE
%token <Lexing.position> SQRT
%token <Lexing.position> FRAC
%token <Lexing.position> SEPARATOR
%token <Lexing.position * float> REAL
%token <Lexing.position * int> INTEGER
%token <Lexing.position * int> DIGIT
%token <Lexing.position * string> VARIABLE
%token <Lexing.position * string> CHAR
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
%token <Lexing.position> EQUIV
%token <Lexing.position * string> TEXT
%token <Lexing.position> SUM

%start <Ast.Latex.t option> start
%start <Ast.Math.t option> math_mode
%start <Ast.Math.t list option> multiline

%type <Ast.Latex.t> content

%%

start:
| node = latex; EOF { Some node }
| EOF { None }

latex:
| all = content*; { {Ast.Node.pos = Lexing.dummy_pos; value = Ast.Latex.Latex all} }

content:
| env = environment { env }
| text = word { text } 
| math = MATHMODE { {Ast.Node.pos = fst math; value = Ast.Latex.Mathmode (snd math)} } 
| math = MULTILINE { {Ast.Node.pos = fst math; value = Ast.Latex.Multiline (snd math)} } 
| command = command { command } 

(* TODO: convert into variant for more semantic information *)
word:
| word_data = WORD; { {Ast.Node.pos = fst word_data; value = Ast.Latex.Word (snd word_data)} }
| data = LINE_BREAK { {Ast.Node.pos = data; value = Ast.Latex.Word "\n"} }
| data = WHITESPACE { {Ast.Node.pos = data; value = Ast.Latex.Word " "} }
| data = COMMA { {Ast.Node.pos = data; value = Ast.Latex.Word ","} }
| data = PIPE { {Ast.Node.pos = data; value = Ast.Latex.Word "|"} }
| data = LEFT_PAREN { {Ast.Node.pos = data; value = Ast.Latex.Word "("} }
| data = RIGHT_PAREN { {Ast.Node.pos = data; value = Ast.Latex.Word ")"} }
| data = LEFT_BRACKET { {Ast.Node.pos = data; value = Ast.Latex.Word "["} }
| data = RIGHT_BRACKET { {Ast.Node.pos = data; value = Ast.Latex.Word "]"} }
| data = EQ { {Ast.Node.pos = data; value = Ast.Latex.Word "="} }
| data = AMPERSAND { {Ast.Node.pos = data; value = Ast.Latex.Word "&"} }

(* a dumb hack to prevent the parser from failing when "]" is located inside of bracketed command arguments *)
(* otherwise, in something like \command[asdf], the parser would recognize the "]" not as the end of the grouping but as a word instead *)
latex_no_rbracket:
| all = content1*; { {Ast.Node.pos = Lexing.dummy_pos; value = Ast.Latex.Latex all} }
content1:
| env = environment { env }
 (* below leads to a shift/reduce conflict, but menhir seems to work fine anyway *)
| text = word_no_rbracket { text } 
| math = MATHMODE { {Ast.Node.pos = fst math; value = Ast.Latex.Mathmode (snd math)} } 
| command = command { command } 
word_no_rbracket:
| word_data = WORD; { {Ast.Node.pos = fst word_data; value = Ast.Latex.Word (snd word_data)} }
| data = LINE_BREAK { {Ast.Node.pos = data; value = Ast.Latex.Word "\n"} }
| data = WHITESPACE { {Ast.Node.pos = data; value = Ast.Latex.Word " "} }
| data = COMMA { {Ast.Node.pos = data; value = Ast.Latex.Word ","} }
| data = PIPE { {Ast.Node.pos = data; value = Ast.Latex.Word "|"} }
| data = LEFT_PAREN { {Ast.Node.pos = data; value = Ast.Latex.Word "("} }
| data = RIGHT_PAREN { {Ast.Node.pos = data; value = Ast.Latex.Word ")"} }
| data = LEFT_BRACKET { {Ast.Node.pos = data; value = Ast.Latex.Word "["} }
| data = EQ { {Ast.Node.pos = data; value = Ast.Latex.Word "="} }
| data = AMPERSAND { {Ast.Node.pos = data; value = Ast.Latex.Word "&"} }

command:
| command = COMMAND; args = command_args; { {Ast.Node.pos = fst command; value = Ast.Latex.Command (snd command, args) } } 

command_args:
| LEFT_BRACKET; contents = latex_no_rbracket; RIGHT_BRACKET; rest = curly_args* { contents :: rest }
| rest = curly_args* { rest }

curly_args:
| LEFT_CURLY; contents = latex; RIGHT_CURLY { contents }

environment:
| name1 = BEGIN; args = command_args; body = content+; END; { {Ast.Node.pos = fst name1; value = Ast.Latex.Environment (snd name1, args, body)} }

(* TODO: figure out a better way of dealing with whitespace *)
(* for now, I'm just doing whitespace right-recursively to appease the parser *)
math_mode:
| WHITESPACE*; relation = relation; EOF { Some relation }
| WHITESPACE*; statement = statement; EOF { Some statement }
| EOF { None }

multiline:
| WHITESPACE*; relations = multiline_aux+; EOF { Some relations }
| EOF { None }

multiline_aux:
| relation = relation; SEPARATOR*; WHITESPACE*; { relation }

(* just specify precedence as part of the grammar bc idk exactly how menhir precedence annotations work *)
statement:
(* forall X, Y *)
| FORALL; WHITESPACE*; rel = relation; COMMA; WHITESPACE*; next = relation; { Ast.Math.Forall (rel, next) }
| FORALL; WHITESPACE*; rel = relation; COMMA; WHITESPACE*; next = statement; { Ast.Math.Forall (rel, next) }
(* exists X : Y *)
| EXISTS; WHITESPACE*; rel = relation; SUCHTHAT; WHITESPACE*; next = relation; { Ast.Math.Exists (rel, Ast.Math.Suchthat next) }
| EXISTS; WHITESPACE*; rel = relation; SUCHTHAT; WHITESPACE*; next = statement; { Ast.Math.Exists (rel, Ast.Math.Suchthat next) }

relation:
| rel = relation1; { rel }

relation1:
(* | lhs = relation1; spacing*; IFF; WHITESPACE*; rhs = relation2; { Ast.Math.Logic (lhs, Ast.Math.Iff, rhs) } *)
(* | lhs = relation1; spacing*; IMPLIES; WHITESPACE*; rhs = relation2; { Ast.Math.Logic (lhs, Ast.Math.Implies, rhs) } *)
(* to support chaining *)
(* Note: this results in the relation list being reversed - it should be un-reversed when walking the AST *)
(* It's possible there's a better way to do this in a way that still appeases the parser *)
| lhs = relation1; spacing*; IMPLIES; WHITESPACE*; spacing*; rhs = relation2; spacing*; {
    match lhs with
    | Logic (x, y) -> Logic (x, (Ast.Math.Implies, rhs) :: y)
    | x -> Logic(x, [(Ast.Math.Implies, rhs)])
  }
| lhs = relation1; spacing*; IFF; WHITESPACE*; spacing*; rhs = relation2; spacing*; {
    match lhs with
    | Logic (x, y) -> Logic (x, (Ast.Math.Iff, rhs) :: y)
    | x -> Logic(x, [(Ast.Math.Iff, rhs)])
  }
| rel = relation2 { rel }

relation2:
| lhs = relation2; LAND; WHITESPACE*; rhs = relation3; { Ast.Math.LogicOp (lhs, Ast.Math.And, rhs) }
| lhs = relation2; LOR; WHITESPACE*; rhs = relation3; { Ast.Math.LogicOp (lhs, Ast.Math.Or, rhs) }
(* grouping *)
| LEFT_PAREN; WHITESPACE*; rel = relation1; RIGHT_PAREN; WHITESPACE*; { Ast.Math.Grouping rel }
| LEFT_CURLY; WHITESPACE*; rel = relation1; RIGHT_CURLY; WHITESPACE*; { Ast.Math.Grouping rel }
| LEFT_BRACKET; WHITESPACE*; rel = relation1; RIGHT_BRACKET; WHITESPACE*; { Ast.Math.Grouping rel }
| rel = relation3 { rel }

relation3:
(* to support chaining *)
(* Note: this results in the relation list being reversed - it should be un-reversed when walking the AST *)
| lhs = relation3; spacing*; rel = rel; spacing*; rhs = expr; spacing*; {
    match lhs with
    | Relation (x, y) -> Relation (x, (rel, rhs) :: y)
    | x -> Relation(x, [(rel, rhs)])
  }
| expr = expr; { expr }
(* | lhs = expr; TEXT?; SEPARATOR?; rels = relation3_aux+; { Ast.Math.Relation (lhs, rels) } *)

spacing:
| TEXT; WHITESPACE* {}
| SEPARATOR; WHITESPACE* {}

(* relation3_aux: *)
(* | rel = rel; rhs = expr { (rel, rhs) } *)

(* precedence (high to low):
  - literals, implicit multiplication, function application
  - times
  - plus, minus
  - set union, set intersection
  Relations:
  - set_in, set_notin
  - subset, ssubset, superset, ssuperset
  - implies, iff
  - and, or
  Statements:
  - forall, exists, such that
  *)

expr:
| infix = infix1; { infix }

infix1:
| lhs = infix1; SET_INTER; WHITESPACE*; rhs = infix2; WHITESPACE*; { Ast.Math.Op (lhs, Ast.Math.Inter, rhs) }
| lhs = infix1; SET_UNION; WHITESPACE*; rhs = infix2; WHITESPACE*; { Ast.Math.Op (lhs, Ast.Math.Union, rhs) }
| infix = infix2; { infix }

infix2:
| lhs = infix2; PLUS; WHITESPACE*; rhs = infix3; WHITESPACE*; { Ast.Math.Op (lhs, Ast.Math.Plus, rhs) }
| lhs = infix2; MINUS; WHITESPACE*; rhs = infix3; WHITESPACE*; { Ast.Math.Op (lhs, Ast.Math.Minus, rhs) }
| infix = infix3; { infix }

infix3:
| lhs = infix3; TIMES; WHITESPACE*; rhs = unary; WHITESPACE*; { Ast.Math.Op (lhs, Ast.Math.Times, rhs) }
| unary = unary; { unary }

unary:
| MINUS; rhs = literal; { Ast.Math.Unary (Ast.Math.Negate, rhs) }
| LNOT; WHITESPACE*; rhs = literal; { Ast.Math.Unary (Ast.Math.Not, rhs) }
(* absolute value *)
| PIPE; WHITESPACE*; expr = expr; PIPE; WHITESPACE*; { Ast.Math.Unary (Ast.Math.Abs, expr) }
| rhs = tuple { rhs }

tuple:
| lhs = literal; rest = tuple_aux+; { Ast.Math.Tuple (lhs :: rest) }
| lhs = literal { lhs }

tuple_aux:
| COMMA; WHITESPACE*; rhs = literal; { rhs }

(* TODO: make each char a separate variable - e.g. abc = a * b * c, not one variable called abc *)
literal:
(* allow interpreting concatenation as multiplication *)
| lhs = literal; rhs = literal; { Ast.Math.Op (lhs, Ast.Math.Times, rhs )}
(* allow brackets to be excluded for single-digit superscript/subscript *)
| lhs = literal; CARET; rhs = DIGIT; WHITESPACE*; { Ast.Math.Superscript (lhs, Ast.Math.IntLiteral (snd rhs))}
| lhs = literal; CARET; rhs = CHAR; WHITESPACE*; { Ast.Math.Superscript (lhs, Ast.Math.Variable (snd rhs))}
| lhs = literal; CARET; LEFT_CURLY; WHITESPACE*; rhs = expr; RIGHT_CURLY; WHITESPACE* { Ast.Math.Superscript (lhs, rhs)}
| lhs = literal; UNDERSCORE; rhs = DIGIT; WHITESPACE*; { Ast.Math.Subscript (lhs, Ast.Math.IntLiteral (snd rhs))}
| lhs = literal; UNDERSCORE; rhs = CHAR; WHITESPACE*; { Ast.Math.Subscript (lhs, Ast.Math.Variable (snd rhs))}
| lhs = literal; UNDERSCORE; LEFT_CURLY; WHITESPACE*; rhs = expr; RIGHT_CURLY; WHITESPACE* { Ast.Math.Subscript (lhs, rhs)}
(* numeric literals *)
| num = DIGIT; WHITESPACE*; { Ast.Math.IntLiteral (snd num) }
| num = INTEGER; WHITESPACE*; { Ast.Math.IntLiteral (snd num) }
| num = REAL; WHITESPACE*; { Ast.Math.FloatLiteral (snd num) }
(* variables *)
| var = VARIABLE; WHITESPACE*; { Ast.Math.Variable (snd var) }
| var = CHAR; WHITESPACE*; { Ast.Math.Variable (snd var) }
| f = function_call; { f }
(* either a list of expressions or a comprehension *)
| s = set_literal; { s }
(* fraction *)
| SQRT; LEFT_CURLY; WHITESPACE*; lhs = expr; RIGHT_CURLY; WHITESPACE*; { Ast.Math.Unary (Ast.Math.Sqrt, lhs) }
| FRAC; LEFT_CURLY; WHITESPACE*; numerator = expr; RIGHT_CURLY; LEFT_CURLY; WHITESPACE*; denominator = expr; RIGHT_CURLY; WHITESPACE*; { Ast.Math.Op (numerator, Ast.Math.Frac, denominator) }
(* summation *)
| SUM; UNDERSCORE; LEFT_CURLY; WHITESPACE*; rel1 = relation; RIGHT_CURLY; CARET; LEFT_CURLY; WHITESPACE*; rel2 = expr; RIGHT_CURLY; WHITESPACE*; body = expr; { Ast.Math.Summation (rel1, rel2, body) }
(* command with arg *)
| command = COMMAND; LEFT_CURLY; arg = expr; RIGHT_CURLY; WHITESPACE* { Ast.Math.Command ((snd command), Some arg)}
(* command without args *)
| command = COMMAND; WHITESPACE* { Ast.Math.Command ((snd command), None)}
(* loop back to expr if grouping spotted *)
| LEFT_PAREN; WHITESPACE*; expr = expr; RIGHT_PAREN; WHITESPACE*; { Ast.Math.Grouping expr }
| LEFT_CURLY; WHITESPACE*; expr = expr; RIGHT_CURLY; WHITESPACE*; { Ast.Math.Grouping expr }
| LEFT_BRACKET; WHITESPACE*; expr = expr; RIGHT_BRACKET; WHITESPACE*; { Ast.Math.Grouping expr }
(* | text = TEXT; WHITESPACE*; { Ast.Math.Command ("\\text", Some (Ast.Math.Text (snd text))) } *)

function_call:
| lhs = VARIABLE; LEFT_PAREN; WHITESPACE*; rhs = separated_list(comma_sep, expr); RIGHT_PAREN; WHITESPACE*; { Ast.Math.Apply (Ast.Math.Variable (snd lhs), rhs) }

set_literal:
  (* comprehension *)
| SET_OPEN; WHITESPACE*; lhs = expr; SUCHTHAT; WHITESPACE*; rel = relation; SET_CLOSE; WHITESPACE*; { Ast.Math.SetComprehension (lhs, rel) }
| SET_OPEN; WHITESPACE*; lhs = expr; PIPE; WHITESPACE*; rel = relation; SET_CLOSE; WHITESPACE*; { Ast.Math.SetComprehension (lhs, rel) }
  (* list of exprs *)
| SET_OPEN; WHITESPACE*; contents = separated_list(comma_sep, expr); SET_CLOSE; WHITESPACE*; { Ast.Math.SetLiteral contents }

comma_sep:
| COMMA; WHITESPACE*; {}

%inline rel:
| LE; WHITESPACE*; { Ast.Math.Le }
| GE; WHITESPACE*; { Ast.Math.Ge }
| LEQ; WHITESPACE*; { Ast.Math.Leq }
| GEQ; WHITESPACE*; { Ast.Math.Geq }
| EQ; WHITESPACE*; { Ast.Math.Eq }
| SUBSET; WHITESPACE*; { Ast.Math.Subset }
| SUBSETEQ; WHITESPACE*; { Ast.Math.SubsetEq }
| SUPERSET; WHITESPACE*; { Ast.Math.Superset }
| SUPERSETEQ; WHITESPACE*; { Ast.Math.SupersetEq }
| SET_IN; WHITESPACE*; { Ast.Math.In }
| SET_NOTIN; WHITESPACE*; { Ast.Math.NotIn }
| EQUIV; UNDERSCORE; d = DIGIT; WHITESPACE* { Ast.Math.Equiv (Ast.Math.IntLiteral (snd d)) }
| EQUIV; UNDERSCORE; d = CHAR; WHITESPACE* { Ast.Math.Equiv (Ast.Math.Variable (snd d)) }
| EQUIV; UNDERSCORE; LEFT_CURLY; e = expr; RIGHT_CURLY; WHITESPACE* { Ast.Math.Equiv e }
