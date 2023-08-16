{
  open Parser

  exception LexError of string

  let math_delimiter = function
  | "$" -> "$"
  | "$$" -> "$$"
  | "\\begin{math}" -> "\\end{math}"
  | "\\begin{equation}" -> "\\end{equation}"
  | "\\begin{align*}" -> "\\end{align*}"
  | "\\[" -> "\\]"
  | s -> raise (LexError (Format.sprintf "Invalid delimiter: %s" s))

}

let linebreak = '\r' | '\n' | "\r\n"
let whitespace = [' ' '\t']+

let linecomment = '%' [^ '\r' '\n']*
let word = [^ ' ' '\t' '%' '{' '}' ',' '$' '[' ']' '(' ')' '=' '|' '\\']+
let text = [^ ' ' '\n' '\r' '\t' '%' '{' '}' ',' '$' '[' ']' '(' ')' '=' '|' '\\']+

let dollar2 = '$' '$'
let dollar = '$'

let alpha = ['a'-'z' 'A'-'Z']
let not_alpha = [^ 'a'-'z' 'A'-'Z']
let command = '\\' not_alpha | '\\' alpha+
let variable = alpha alpha+

let digit = ['0'-'9']
let integer = digit digit+
let real = ['0'-'9']+ '.' ['0'-'9']+

let begin_env = "\\begin{" ['a'-'z' 'A'-'Z']+ '}'
let end_env = "\\end{" ['a'-'z' 'A'-'Z']+ '}'

let math_sep = "\\\\"
(* does not check for matching begin/end, assume latex lsp will catch that *)
let begin_math = "$$" | "$" | "\\[" | "\\begin{math}" | "\\begin{equation}"
let end_math = "$$" | "$" | "\\]" | "\\end{math}" | "\\end{equation}"
let begin_multiline = "\\begin{align*}"
let end_multiline = "\\end{align*}"

rule token =
  parse
  | linebreak   { Lexing.new_line lexbuf; LINE_BREAK lexbuf.lex_curr_p }
  | whitespace  { WHITESPACE lexbuf.lex_curr_p }
  (* | linecomment { LINE_COMMENT (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) } *)
  (* just ignore line comments for now *)
  | linecomment { token lexbuf } 
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
  | begin_multiline  { 
    let (a, b) = mathmode (Buffer.create 80) (math_delimiter (Lexing.lexeme lexbuf)) (lexbuf.lex_start_p) lexbuf in
    MULTILINE (a, b)
  }
  | begin_math  { 
    let (a, b) = mathmode (Buffer.create 80) (math_delimiter (Lexing.lexeme lexbuf)) (lexbuf.lex_start_p) lexbuf in
    MATHMODE (a, b)
  }
  | begin_env   { BEGIN (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | end_env     { END (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | command     { COMMAND (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | text        { WORD (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | eof         { EOF lexbuf.lex_curr_p }
  | _           { raise (LexError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
and mathmode buf delim start =
  parse
  | linebreak   { Lexing.new_line lexbuf; mathmode buf delim start lexbuf}
  | whitespace  { Buffer.add_string buf (Lexing.lexeme lexbuf); mathmode buf delim start lexbuf }
  | linecomment { mathmode buf delim start lexbuf}
  | end_multiline{ if (String.equal (Lexing.lexeme lexbuf) delim) then
                    (start, Buffer.contents buf)
                  else (
                    Buffer.add_string buf (Lexing.lexeme lexbuf);
                    mathmode buf delim start lexbuf)
                  }
  | end_math    { if (String.equal (Lexing.lexeme lexbuf) delim) then
                    (start, Buffer.contents buf)
                  else (
                    Buffer.add_string buf (Lexing.lexeme lexbuf);
                    mathmode buf delim start lexbuf)
                  }
  | [^ '$' '\n' '\r' '\t' ' ']+    { Buffer.add_string buf (Lexing.lexeme lexbuf); mathmode buf delim start lexbuf}
  | _           { raise (LexError ("Unexpected char in math mode: " ^ Lexing.lexeme lexbuf)) }
  | eof         { raise (LexError (Format.sprintf "Math mode is not terminated")) }
and math_token =
  parse
  (* syntax *)
  | linebreak   { Lexing.new_line lexbuf; LINE_BREAK lexbuf.lex_curr_p }
  | math_sep    { SEPARATOR lexbuf.lex_curr_p }
  | '&'         { math_token lexbuf }
  | whitespace  { WHITESPACE lexbuf.lex_curr_p }
  | "\\displaystyle"  { WHITESPACE lexbuf.lex_curr_p }
  | '{'         { LEFT_CURLY lexbuf.lex_curr_p }
  | '}'         { RIGHT_CURLY lexbuf.lex_curr_p }
  | '['         { LEFT_BRACKET lexbuf.lex_curr_p}
  | ']'         { RIGHT_BRACKET lexbuf.lex_curr_p}
  | '('         { LEFT_PAREN lexbuf.lex_curr_p}
  | ')'         { RIGHT_PAREN lexbuf.lex_curr_p}
  | "\\left"    { LEFT_PAREN lexbuf.lex_curr_p}
  | "\\right"   { RIGHT_PAREN lexbuf.lex_curr_p}
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
  | "\\land"    { LAND lexbuf.lex_curr_p}
  | "\\lor"     { LOR lexbuf.lex_curr_p}
  | "\\neg"     { LNOT lexbuf.lex_curr_p}
  | "\\implies" { IMPLIES lexbuf.lex_curr_p}
  | "\\iff"     { IFF lexbuf.lex_curr_p}
  | "\\forall"  { FORALL lexbuf.lex_curr_p}
  | "\\exists"  { EXISTS lexbuf.lex_curr_p}
  | "\\mid"     { SUCHTHAT lexbuf.lex_curr_p}
  | "\\:"       { SUCHTHAT lexbuf.lex_curr_p}
  | "s.t."      { SUCHTHAT lexbuf.lex_curr_p}
  (* number theory *)
  | "\\equiv"   { EQUIV lexbuf.lex_curr_p}
  (* misc *)
  | "\\sum"     { SUM lexbuf.lex_curr_p}
  | '_'         { UNDERSCORE lexbuf.lex_curr_p}
  | '^'         { CARET lexbuf.lex_curr_p}
  | real        { REAL (lexbuf.lex_curr_p, float_of_string (Lexing.lexeme lexbuf)) }
  | integer     { INTEGER (lexbuf.lex_curr_p, int_of_string (Lexing.lexeme lexbuf)) }
  | digit       { DIGIT (lexbuf.lex_curr_p, int_of_string (Lexing.lexeme lexbuf)) }
  | "\\text{"   { 
    let contents = math_text (Buffer.create 80) lexbuf in
    if (String.contains contents ' ') then
      TEXT (lexbuf.lex_curr_p, contents)
    else
      VARIABLE (lexbuf.lex_curr_p, contents)
  }
  | "\\textit{" { TEXT (lexbuf.lex_curr_p, math_text (Buffer.create 80) lexbuf) }
  | "\\textrm{" { TEXT (lexbuf.lex_curr_p, math_text (Buffer.create 80) lexbuf) }
  | "\\texttt{" { VARIABLE (lexbuf.lex_curr_p, math_text (Buffer.create 80) lexbuf) }
  | alpha       { CHAR (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | variable    { VARIABLE (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | command     { COMMAND (lexbuf.lex_curr_p, Lexing.lexeme lexbuf) }
  | _           { raise (LexError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof         { EOF lexbuf.lex_curr_p }
and math_text buf =
  parse
  | '}'         { Buffer.contents buf }
  | [^ '}']     { Buffer.add_string buf (Lexing.lexeme lexbuf); math_text buf lexbuf}
  | _           { raise (LexError ("Unexpected char in \\text: " ^ Lexing.lexeme lexbuf)) }
  | eof         { raise (LexError ("Text command mode is not closed")) }
