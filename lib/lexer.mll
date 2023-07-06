{
  open Parser

  exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"
let linebreak = newline+

let whitespace = [' ' '\t']+

let linecomment = '%' [^ '\r' '\n']*
let word = [^ ' ' '\t' '%' '{' '}' ',' '$' '[' ']' '(' ')' '=' '|' '\\']+
let text = [^ ' ' '\n' '\r' '\t' '%' '{' '}' ',' '$' '[' ']' '(' ')' '=' '|' '\\']+

let dollar2 = '$' '$'
let dollar = '$'

let alpha = ['a'-'z' 'A'-'Z']
let not_alpha = [^ 'a'-'z' 'A'-'Z']
let command = '\\' not_alpha | '\\' alpha+

let integer = ['0'-'9']+

let begin_env = "\\begin{" ['a'-'z' 'A'-'Z']+ '}'
let end_env = "\\end{" ['a'-'z' 'A'-'Z']+ '}'

let math_sep = "\\\\" | '&'
(* does not check for matching begin/end, assume latex lsp will catch that *)
let begin_math = '$' '$'? | "\\[" | "\\begin{math}"
let end_math = '$' '$'? | "\\]" | "\\end{math}"

rule token =
  parse
  | linebreak   { LINE_BREAK lexbuf.lex_curr_p }
  | whitespace  { WHITESPACE lexbuf.lex_curr_p }
  | linecomment { LINE_COMMENT (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | '{'         { LEFT_CURLY lexbuf.lex_curr_p }
  | '}'         { RIGHT_CURLY lexbuf.lex_curr_p }
  | '['         { LEFT_BRACKET lexbuf.lex_curr_p}
  | ']'         { RIGHT_BRACKET lexbuf.lex_curr_p}
  | '('         { LEFT_PAREN lexbuf.lex_curr_p}
  | ')'         { RIGHT_PAREN lexbuf.lex_curr_p}
  | ','         { COMMA lexbuf.lex_curr_p}
  | '|'         { PIPE lexbuf.lex_curr_p}
  | '='         { EQ lexbuf.lex_curr_p}
  | '&'         { AMPERSAND lexbuf.lex_curr_p}
  | begin_math  { MATHMODE (lexbuf.lex_curr_p, mathmode (Buffer.create 80) lexbuf) }
  | begin_env   { BEGIN (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | end_env     { END (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | command     { COMMAND (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | word        { WORD (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | eof         { EOF lexbuf.lex_curr_p }
  | _           { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
and mathmode buf =
  parse
  | end_math    { Buffer.contents buf }
  | [^ '$']+    { Buffer.add_string buf (Lexing.lexeme lexbuf); mathmode buf lexbuf}
  | _           { raise (SyntaxError ("Unexpected char in math mode: " ^ Lexing.lexeme lexbuf)) }
  | eof         { raise (SyntaxError ("Math mode is not terminated")) }
and math_token =
  parse
  (* syntax *)
  | math_sep    { SEPARATOR lexbuf.lex_curr_p }
  | whitespace  { WHITESPACE lexbuf.lex_curr_p }
  | '{'         { LEFT_CURLY lexbuf.lex_curr_p }
  | '}'         { RIGHT_CURLY lexbuf.lex_curr_p }
  | '['         { LEFT_BRACKET lexbuf.lex_curr_p}
  | ']'         { RIGHT_BRACKET lexbuf.lex_curr_p}
  | '('         { LEFT_PAREN lexbuf.lex_curr_p}
  | ')'         { RIGHT_PAREN lexbuf.lex_curr_p}
  | ','         { COMMA lexbuf.lex_curr_p}
  | '|'         { PIPE lexbuf.lex_curr_p}
  | '='         { EQ lexbuf.lex_curr_p}
  | '&'         { AMPERSAND lexbuf.lex_curr_p}
  (* numeric ops *)
  | '+'         { PLUS lexbuf.lex_curr_p}
  | '-'         { MINUS lexbuf.lex_curr_p}
  | '*'         { TIMES lexbuf.lex_curr_p}
  | "\\cdot"    { TIMES lexbuf.lex_curr_p}
  | '<'         { LE lexbuf.lex_curr_p}
  | '>'         { GE lexbuf.lex_curr_p}
  | "\\leq"     { LEQ lexbuf.lex_curr_p}
  | "\\geq"     { GEQ lexbuf.lex_curr_p}
  | "\\frac"    { FRAC lexbuf.lex_curr_p}
  (* set ops *)
  | "\\{"       { SET_OPEN lexbuf.lex_curr_p }
  | "\\}"       { SET_CLOSE lexbuf.lex_curr_p }
  | "\\in"      { SET_IN lexbuf.lex_curr_p}
  | "\\notin"   { SET_NOTIN lexbuf.lex_curr_p}
  | "\\cup"     { SET_UNION lexbuf.lex_curr_p}
  | "\\cap"     { SET_INTER lexbuf.lex_curr_p}
  | "\\subset"  { SUBSET lexbuf.lex_curr_p}
  | "\\subseteq"{ SUBSETEQ lexbuf.lex_curr_p}
  | "\\superset"  { SUPERSET lexbuf.lex_curr_p}
  | "\\superseteq"{ SUPERSETEQ lexbuf.lex_curr_p}
  (* logic *)
  | "\\implies" { IMPLIES lexbuf.lex_curr_p}
  | "\\iff"     { IFF lexbuf.lex_curr_p}
  | "\\forall"  { FORALL lexbuf.lex_curr_p}
  | "\\exists"  { EXISTS lexbuf.lex_curr_p}
  | "\\mid"     { SUCHTHAT lexbuf.lex_curr_p}
  | "\\:"       { SUCHTHAT lexbuf.lex_curr_p}
  | "s.t."      { SUCHTHAT lexbuf.lex_curr_p}
  (* misc *)
  | '_'         { UNDERSCORE lexbuf.lex_curr_p}
  | '^'         { CARET lexbuf.lex_curr_p}
  | integer     { INTEGER (lexbuf.lex_curr_p, int_of_string (Lexing.lexeme lexbuf)) }
  | alpha+      { VARIABLE (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | command     { COMMAND (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | _           { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof         { EOF lexbuf.lex_curr_p }
