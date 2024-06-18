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

module I_latex = Latex_parser.MenhirInterpreter
module I_math = Math_parser.MenhirInterpreter

(* using the menhir incremental api *)
(* for reference: https://www.baturin.org/blog/declarative-parse-error-reporting-with-menhir/ *)
let latex_parse_error env =
  match I_latex.stack env with
  | lazy Nil -> "Invalid syntax"
  | lazy (Cons (I_latex.Element (state, _, _, _), _)) ->
    let n = I_latex.number state in
      Format.sprintf "Latex error state %i: %s" n (Latex_parser_messages.message n)

let math_parse_error env =
  match I_math.stack env with
  | lazy Nil -> "Invalid syntax"
  | lazy (Cons (I_math.Element (state, _, _, _), _)) ->
    let n = I_math.number state in
      Format.sprintf "Math error state %i: %s" n (Math_parser_messages.message n)

let rec parse_latex lexbuf checkpoint =
  let open Lexing in
  match checkpoint with
  | I_latex.InputNeeded _env ->
      let token = Latex_lexer.token lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I_latex.offer checkpoint (token, startp, endp) in
      parse_latex lexbuf checkpoint
  | I_latex.Shifting _
  | I_latex.AboutToReduce _ ->
      let checkpoint = I_latex.resume checkpoint in
      parse_latex lexbuf checkpoint
  | I_latex.HandlingError _env ->
    let pos = lexbuf.lex_curr_p in
    let msg = latex_parse_error _env in
    raise (Error (msg, Some pos))
  | I_latex.Accepted v -> v
  | I_latex.Rejected ->
       raise (Error ("Invalid syntax", None))

let parse_latex_lexbuf lexbuf =
  let open Lexing in
  let result = try parse_latex lexbuf (Latex_parser.Incremental.start lexbuf.lex_curr_p) with
  (* let result = try Parser.start Lexer.token lexbuf with *)
  | Latex_lexer.LexError msg -> (
    raise (Error (sprintf "Lex Error: %s" msg, Some lexbuf.lex_curr_p))
    (* fprintf stderr "Lex Error (%a): %s\n" print_position lexbuf msg; *)
  )
  | Latex_parser.Error -> (
    (* fprintf stderr "Parse Error (%a)\n" print_position lexbuf; *)
    raise (Error ("Parse Error", Some lexbuf.lex_curr_p))
  )
  in
  result

(* let parse_latex str = parse_latex_lexbuf (Lexing.from_string str) *)

let parse_latex_file filename = 
  let fin = try In_channel.create filename with
  | Sys_error msg -> raise (Error ((Format.sprintf "File not found: %s" msg), None))
  in
  let lexbuf = Lexing.from_channel fin in
  Lexing.set_filename lexbuf filename;
  parse_latex_lexbuf lexbuf

let rec parse_math lexbuf checkpoint =
  let open Lexing in
  match checkpoint with
  | I_math.InputNeeded _env ->
      let token = Math_lexer.math_token lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I_math.offer checkpoint (token, startp, endp) in
      parse_math lexbuf checkpoint
  | I_math.Shifting _
  | I_math.AboutToReduce _ ->
      let checkpoint = I_math.resume checkpoint in
      parse_math lexbuf checkpoint
  | I_math.HandlingError _env ->
    let pos = lexbuf.lex_curr_p in
    let msg = math_parse_error _env in
    raise (Error (msg, Some pos))
  | I_math.Accepted v -> v
  | I_math.Rejected ->
       raise (Error ("Invalid syntax", None))

let parse_math ast =
  match ast with
  | Latex.Mathmode str -> (
    let lexbuf = Lexing.from_string str in
    let result = try parse_math lexbuf (Math_parser.Incremental.math_mode lexbuf.lex_curr_p) with
      | Math_lexer.LexError msg -> (
        raise (Error (sprintf "Lex Error: %s" msg, Some lexbuf.lex_curr_p))
      )
      | Math_parser.Error -> (
        raise (Error ("Parse Error", Some lexbuf.lex_curr_p))
      )
    in
    match result with
    | Some x -> [x]
    | None -> []
  )
  | Latex.Multiline str -> (
    let lexbuf = Lexing.from_string str in
    let result = try parse_math lexbuf (Math_parser.Incremental.multiline lexbuf.lex_curr_p) with
      | Math_lexer.LexError msg -> (
        raise (Error (sprintf "Lex Error: %s" msg, Some lexbuf.lex_curr_p))
      )
      | Math_parser.Error -> (
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

(* let message = Parser_messages.message *)

type result = (int, Math.t) Hashtbl.t

type pattern = 
  | Word of string
  | Option of pattern
  | Sequence of pattern list
  | Any of pattern list
  | Variable of int
  | Relation of int
  | Definition of int
[@@deriving eq, show, sexp, hash, ord]

let def = Sequence [Any [Word "choose"; Word "consider"; Word "define"]; Relation 0]

let match_with (latex: Ast.Latex.t) (pat: pattern) =
  let (mappings: result) = Hashtbl.create (module Int) in
  let rec recurse (node: Ast.Latex.t) (pat: pattern) = 
    match (node, pat) with
    | ({ pos = _; value = Word x}, Word y) -> String.equal (String.lowercase x) (String.lowercase y)
    | ({ pos = _; value = _}, Option p) -> recurse node p
    | ({ pos = _; value = _}, Sequence []) -> true
    (* ignore whitespace and newlines *)
    | ({ pos = p; value = Latex ({ pos = _; value = Whitespace} :: tl)}, _) -> recurse { pos = p; value = Latex tl } pat
    | ({ pos = p; value = Latex ({ pos = _; value = Newline} :: tl)}, _) -> recurse { pos = p; value = Latex tl } pat
    (* match sequences *)
    | ({ pos = p; value = Latex (hd :: tl)}, Sequence (fst :: rest)) -> (
      (* Format.printf "Matching %a with %s\n" Latex.pp node (show_pattern pat); *)
      if recurse hd fst then (
        recurse {pos = p; value = Latex tl} (Sequence rest)
      )
      else
        false
    )
    | ({ pos = _; value = _}, Any []) -> false
    | ({ pos = _; value = _}, Any (hd :: tl)) -> (
      if recurse node hd then
        true
      else
        recurse node (Any tl)
    )
    | ({ pos = _; value = Mathmode x}, Variable id) -> (
      let result = parse_math (Mathmode x) in
      match result with
      | [Variable t] -> (
        Hashtbl.set mappings ~key:id ~data:(Variable t);
        true
      )
      | _ -> false
    )
    | ({ pos = _; value = Mathmode x}, Relation id) -> (
      let result = parse_math (Mathmode x) in
      match result with
      | [Relation _ as rel] -> (
        Hashtbl.set mappings ~key:id ~data:rel;
        true
      )
      | _ -> false
    )
    | _ -> Format.printf "Could not match %a with %s\n" Latex.pp node (show_pattern pat); false
  in
  if recurse latex pat then
    Some mappings
  else
    None
