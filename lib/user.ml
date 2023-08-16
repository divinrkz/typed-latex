(* a user interface for easily parsing latex *)
(* menhir/ocamllex should be hidden by this abstraction *)
open Core
open Ast

(* our own abstraction on top of Lexer.LexError, Parser.Error, and Type Error *)
(* TODO: better parse errors (use menhir incremental api?) *)
(* TODO: create details api for error messages with context/reasoning *)
exception Error of string * Lexing.position option

let print_position pos =
  let open Lexing in
  sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let error_message e =
  match e with
  | Error (msg, Some pos) -> sprintf "(%s) %s\n"( print_position pos) msg
  | Error (msg, None) -> sprintf "%s\n" msg
  | _ -> raise e

module I = Parser.MenhirInterpreter

let pp_symbol: type a. Format.formatter -> a I.symbol -> unit = fun formatter -> function
  | T I.T_LINE_BREAK -> Format.fprintf formatter "LINE_BREAK"
  | T I.T_WHITESPACE -> Format.fprintf formatter "WHITESPACE"
  | T I.T_LINE_COMMENT -> Format.fprintf formatter "LINE_COMMENT"
  | T I.T_LEFT_CURLY -> Format.fprintf formatter "LEFT_CURLY"
  | T I.T_LEFT_BRACKET -> Format.fprintf formatter "LEFT_BRACKET"
  | T I.T_LEFT_PAREN -> Format.fprintf formatter "LEFT_PAREN"
  | T I.T_RIGHT_CURLY -> Format.fprintf formatter "RIGHT_CURLY"
  | T I.T_RIGHT_BRACKET -> Format.fprintf formatter "RIGHT_BRACKET"
  | T I.T_RIGHT_PAREN -> Format.fprintf formatter "RIGHT_PAREN"
  | T I.T_COMMA -> Format.fprintf formatter "COMMA"
  | T I.T_AMPERSAND -> Format.fprintf formatter "AMPERSAND"
  | T I.T_PIPE -> Format.fprintf formatter "PIPE"
  | T I.T_WORD -> Format.fprintf formatter "WORD"
  | T I.T_COMMAND -> Format.fprintf formatter "COMMAND"
  | T I.T_EOF -> Format.fprintf formatter "EOF"
  | T I.T_BEGIN -> Format.fprintf formatter "BEGIN"
  | T I.T_END -> Format.fprintf formatter "END"
  | T I.T_MATHMODE -> Format.fprintf formatter "MATHMODE"
  | T I.T_MULTILINE -> Format.fprintf formatter "MULTILINE"
  | T I.T_EQ -> Format.fprintf formatter "EQ"
  | T I.T_PLUS -> Format.fprintf formatter "PLUS"
  | T I.T_MINUS -> Format.fprintf formatter "MINUS"
  | T I.T_TIMES -> Format.fprintf formatter "TIMES"
  | T I.T_CARET -> Format.fprintf formatter "CARET"
  | T I.T_UNDERSCORE -> Format.fprintf formatter "UNDERSCORE"
  | T I.T_FRAC -> Format.fprintf formatter "FRAC"
  | T I.T_SEPARATOR -> Format.fprintf formatter "SEPARATOR"
  | T I.T_REAL -> Format.fprintf formatter "REAL"
  | T I.T_INTEGER -> Format.fprintf formatter "INTEGER"
  | T I.T_DIGIT -> Format.fprintf formatter "DIGIT"
  | T I.T_VARIABLE -> Format.fprintf formatter "VARIABLE"
  | T I.T_CHAR -> Format.fprintf formatter "CHAR"
  | T I.T_LE -> Format.fprintf formatter "LE"
  | T I.T_GE -> Format.fprintf formatter "GE"
  | T I.T_LEQ -> Format.fprintf formatter "LEQ"
  | T I.T_GEQ -> Format.fprintf formatter "GEQ"
  | T I.T_SET_IN -> Format.fprintf formatter "SET_IN"
  | T I.T_SET_OPEN -> Format.fprintf formatter "SET_OPEN"
  | T I.T_SET_CLOSE -> Format.fprintf formatter "SET_CLOSE"
  | T I.T_SET_NOTIN -> Format.fprintf formatter "SET_NOTIN"
  | T I.T_SET_UNION -> Format.fprintf formatter "SET_UNION"
  | T I.T_SET_INTER -> Format.fprintf formatter "SET_INTER"
  | T I.T_SUBSET -> Format.fprintf formatter "SUBSET"
  | T I.T_SUPERSET -> Format.fprintf formatter "SUPERSET"
  | T I.T_SUBSETEQ -> Format.fprintf formatter "SUBSETEQ"
  | T I.T_SUPERSETEQ -> Format.fprintf formatter "SUPERSETEQ"
  | T I.T_IMPLIES -> Format.fprintf formatter "IMPLIES"
  | T I.T_IFF -> Format.fprintf formatter "IFF"
  | T I.T_FORALL -> Format.fprintf formatter "FORALL"
  | T I.T_EXISTS -> Format.fprintf formatter "EXISTS"
  | T I.T_SUCHTHAT -> Format.fprintf formatter "SUCHTHAT"
  | T I.T_LAND -> Format.fprintf formatter "LAND"
  | T I.T_LOR -> Format.fprintf formatter "LOR"
  | T I.T_LNOT -> Format.fprintf formatter "LNOT"
  | T I.T_EQUIV -> Format.fprintf formatter "EQUIV"
  | T I.T_TEXT -> Format.fprintf formatter "TEXT"
  | T I.T_SUM -> Format.fprintf formatter "SUM"
  | T I.T_error -> Format.fprintf formatter "error"
  | N I.N_start -> Format.fprintf formatter "start"
  | N I.N_latex -> Format.fprintf formatter "latex"
  | N I.N_content -> Format.fprintf formatter "content"
  | N I.N_word -> Format.fprintf formatter "word"
  | N I.N_latex_no_rbracket -> Format.fprintf formatter "rbracket"
  | N I.N_content1 -> Format.fprintf formatter "content1"
  | N I.N_word_no_rbracket -> Format.fprintf formatter "rbracket"
  | N I.N_command -> Format.fprintf formatter "command"
  | N I.N_command_args -> Format.fprintf formatter "args"
  | N I.N_curly_args -> Format.fprintf formatter "args"
  | N I.N_environment -> Format.fprintf formatter "environment"
  | N I.N_math_mode -> Format.fprintf formatter "mode"
  | N I.N_multiline -> Format.fprintf formatter "multiline"
  | N I.N_multiline_aux -> Format.fprintf formatter "aux"
  | N I.N_statement -> Format.fprintf formatter "statement"
  | N I.N_relation -> Format.fprintf formatter "relation"
  | N I.N_relation1 -> Format.fprintf formatter "relation1"
  | N I.N_relation2 -> Format.fprintf formatter "relation2"
  | N I.N_relation3 -> Format.fprintf formatter "relation3"
  | N I.N_spacing -> Format.fprintf formatter "spacing"
  | N I.N_expr -> Format.fprintf formatter "expr"
  | N I.N_infix1 -> Format.fprintf formatter "infix1"
  | N I.N_infix2 -> Format.fprintf formatter "infix2"
  | N I.N_infix3 -> Format.fprintf formatter "infix3"
  | N I.N_unary -> Format.fprintf formatter "unary"
  | N I.N_tuple -> Format.fprintf formatter "tuple"
  | N I.N_tuple_aux -> Format.fprintf formatter "aux"
  | N I.N_literal -> Format.fprintf formatter "literal"
  | N I.N_function_call -> Format.fprintf formatter "call"
  | N I.N_set_literal -> Format.fprintf formatter "literal"
  | N I.N_comma_sep -> Format.fprintf formatter "sep"
  | N I.N_list_content_ -> Format.fprintf formatter "list_content_"
  | N I.N_list_spacing_ -> Format.fprintf formatter "list_spacing_"
  | N I.N_list_content1_ -> Format.fprintf formatter "list_content1_"
  | N I.N_list_WHITESPACE_ -> Format.fprintf formatter "list_WHITESPACE_"
  | N I.N_list_curly_args_ -> Format.fprintf formatter "list_curly_args_"
  | N I.N_nonempty_list_content_ -> Format.fprintf formatter "nonempty_list_content_"
  | N I.N_nonempty_list_tuple_aux_ -> Format.fprintf formatter "nonempty_list_tuple_aux_"
  | N I.N_nonempty_list_multiline_aux_ -> Format.fprintf formatter "nonempty_list_multiline_aux_"
  | N I.N_separated_nonempty_list_comma_sep_expr_ -> Format.fprintf formatter "separated_nonempty_list_comma_sep_expr_"
  | N I.N_loption_separated_nonempty_list_comma_sep_expr__ -> Format.fprintf formatter "loption_separated_nonempty_list_comma_sep_expr__"

(* using the menhir incremental api *)
(* for reference: https://www.baturin.org/blog/declarative-parse-error-reporting-with-menhir/ *)
let get_parse_error env =
  match I.stack env with
  | lazy Nil -> "Invalid syntax"
(* using parser.messages file *)
  (* | lazy (Cons (I.Element (state, _, _, _), _)) -> *)
  (*     Parser_messages.message (I.number state) *)
  (* possible errors:
     latex:
     - unmatched environment begin/end
     math:
      - unmatched paren/brace
      - unexpected token (last term)
        - try to figure out what was expected?
     *)
  | lazy (Cons (I.Element (state, _, _, _), _)) -> (
    Format.printf "Incoming symbol: %a\n" pp_symbol (I.incoming_symbol state);
    match I.number state with
    | x -> Format.sprintf "Error state %i" x
  )

let rec parse tokenizer lexbuf checkpoint =
  let open Lexing in
  match checkpoint with
  | I.InputNeeded _env ->
      let token = tokenizer lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse tokenizer lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse tokenizer lexbuf checkpoint
  | I.HandlingError _env ->
    let pos = lexbuf.lex_curr_p in
    let msg = get_parse_error _env in
    raise (Error (msg, Some pos))
  | I.Accepted v -> v
  | I.Rejected ->
       raise (Error ("Invalid syntax", None))

let parse_latex_lexbuf lexbuf =
  let result = try parse Lexer.token lexbuf (Parser.Incremental.start lexbuf.lex_curr_p) with
  (* let result = try Parser.start Lexer.token lexbuf with *)
  | Lexer.LexError msg -> (
    raise (Error (sprintf "Lex Error: %s" msg, Some lexbuf.lex_curr_p))
    (* fprintf stderr "Lex Error (%a): %s\n" print_position lexbuf msg; *)
  )
  | Parser.Error -> (
    (* fprintf stderr "Parse Error (%a)\n" print_position lexbuf; *)
    raise (Error ("Parse Error", Some lexbuf.lex_curr_p))
  )
  in
  result

let parse_latex str = parse_latex_lexbuf (Lexing.from_string str)

let parse_latex_file filename = 
  let fin = try In_channel.create filename with
  | Sys_error msg -> raise (Error ((Format.sprintf "File not found: %s" msg), None))
  in
  let lexbuf = Lexing.from_channel fin in
  Lexing.set_filename lexbuf filename;
  parse_latex_lexbuf lexbuf

(* user needs to handle possible lex/parse error *)
let parse_math ast =
  match ast with
  | Latex.Mathmode str -> (
    Format.printf "mathmode\n";
    let lexbuf = Lexing.from_string str in
    let result = try parse Lexer.math_token lexbuf (Parser.Incremental.math_mode lexbuf.lex_curr_p) with
    (* let result = try Parser.math_mode Lexer.math_token lexbuf with *)
      | Lexer.LexError msg -> (
        raise (Error (sprintf "Lex Error: %s" msg, Some lexbuf.lex_curr_p))
      )
      | Parser.Error -> (
        raise (Error ("Parse Error", Some lexbuf.lex_curr_p))
      )
    in
    match result with
    | Some x -> [x]
    | None -> []
  )
  | Latex.Multiline str -> (
    Format.printf "multiline: \"%s\"\n" str;
    let lexbuf = Lexing.from_string str in
    (* let result = try Parser.multiline Lexer.math_token lexbuf with *)
    let result = try parse Lexer.math_token lexbuf (Parser.Incremental.multiline lexbuf.lex_curr_p) with
      | Lexer.LexError msg -> (
        raise (Error (sprintf "Lex Error: %s" msg, Some lexbuf.lex_curr_p))
      )
      | Parser.Error -> (
        raise (Error ("Parse Error", Some lexbuf.lex_curr_p))
      )
    in
    match result with
    | Some x -> x
    | None -> []
  )
  | _ -> raise (Error ("Trying to parse non-math element as math", None))

let type_check ast =
    (* Format.printf "%a" Ast.Latex.pp ast; *)
    let math = Ast.Latex.get_all_math ast in
    let math_nodes = ref [] in
    List.iter math ~f:(fun ast ->
      let res = parse_math ast in
      math_nodes := List.append !math_nodes res
    );
    Ast.Math.type_check !math_nodes

let message = Parser_messages.message
