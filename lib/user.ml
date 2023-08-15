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

let parse_latex_lexbuf lexbuf =
  let result = try Parser.start Lexer.token lexbuf with
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
    let lexbuf = Lexing.from_string str in
    let result = try Parser.math_mode Lexer.math_token lexbuf with
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
    let lexbuf = Lexing.from_string str in
    let result = try Parser.multiline Lexer.math_token lexbuf with
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
  | _ -> raise (Error ("Trying to parse non-math element as math", Some Lexing.dummy_pos))

let type_check ast =
    (* Format.printf "%a" Ast.Latex.pp ast; *)
    let math = Ast.Latex.get_all_math ast in
    let math_nodes = ref [] in
    List.iter math ~f:(fun ast ->
      let res = parse_math ast in
      math_nodes := List.append !math_nodes res
    );
    (* Format.printf "%a\n" (Format.pp_print_list ~pp_sep:(string_sep ", ") Format.pp_print_string) math_nodes *)
    Ast.Math.type_check !math_nodes
