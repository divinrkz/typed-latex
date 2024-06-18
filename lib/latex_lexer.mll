{
  open Latex_parser

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

let begin_env = "\\begin{" ['a'-'z' 'A'-'Z']+ '}'
let end_env = "\\end{" ['a'-'z' 'A'-'Z']+ '}'

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
  | '.'         { PERIOD lexbuf.lex_curr_p}
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
  | [^ '$' '\n' '\r' '\t' ' ' '\\']+    { Buffer.add_string buf (Lexing.lexeme lexbuf); mathmode buf delim start lexbuf}
  (* ensure backslashes don't get lexed as part of another string (e.g. "asdf\command") *)
  | '\\'        { Buffer.add_string buf (Lexing.lexeme lexbuf); mathmode buf delim start lexbuf}
  | _           { raise (LexError ("Unexpected char in math mode: " ^ Lexing.lexeme lexbuf)) }
  | eof         { raise (LexError (Format.sprintf "Math mode is not terminated")) }
