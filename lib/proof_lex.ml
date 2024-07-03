open Core
open Ast
open User
open Fn
open Util

type proof_token = WordToken of string | MathToken of Math.t

let rec tokenize_rec (working_tokenization : proof_token list list)
    (latex : Latex.t) =
  match User.unwrap_node latex with
  | Latex.Word word -> word_tokenizer working_tokenization word
  | Latex.Latex children ->
      List.fold_right ~f:(flip tokenize_rec) ~init:working_tokenization children
  | Latex.Environment (name, args, children) ->
      environment_tokenizer working_tokenization name args children
  | Latex.Command (name, children) ->
      command_tokenizer working_tokenization name children
  | Latex.Mathmode _ as math_latex ->
      math_tokenizer working_tokenization math_latex
  | Latex.Multiline _ as math_latex ->
      math_tokenizer working_tokenization math_latex
  | Latex.Newline -> working_tokenization
  | Latex.Whitespace -> working_tokenization

and word_tokenizer (working_tokenization : proof_token list list)
    (word : string) =
  match
    String.findi
      ~f:(fun _ char -> List.mem ~equal:Char.equal Util.word_split_chars char)
      word
  with
  | Some (i, char) ->
      let left, right =
        (String.non_stupid_slice word 0 i, String.drop_prefix word (i + 1))
      in
      elementary_word_tokenizer
        (elementary_word_tokenizer
           (word_tokenizer working_tokenization right)
           (String.of_char char))
        left
  | None -> elementary_word_tokenizer working_tokenization word

and elementary_word_tokenizer (working_tokenization : proof_token list list)
    (word : string) =
  if String.equal word "" then working_tokenization
  else if List.mem ~equal:String.equal Util.sentence_split_words word then
    [] :: working_tokenization
  else
    match working_tokenization with
    | head :: tail -> (WordToken word :: head) :: tail
    | [] -> []

and environment_tokenizer (working_tokenization : proof_token list list)
    (name : string) (args : Latex.t list) (children : Latex.t list) =
  let proper_name =
    String.chop_prefix_if_exists ~prefix:"\\begin{"
      (String.chop_suffix_if_exists ~suffix:"}" name)
  in
  match (proper_name, args, children) with
  | ("itemize" | "enumerate"), _, _ ->
      ([] :: List.concat_no_order (List.map ~f:(tokenize_rec [ [] ]) children))
      @ working_tokenization
  | "document", _, _ ->
      List.fold_right ~f:(flip tokenize_rec) ~init:working_tokenization children
  | _, _, _ ->
      List.fold_right ~f:(flip tokenize_rec) ~init:working_tokenization children

and command_tokenizer (working_tokenization : proof_token list list)
    (name : string) (children : Latex.t list) =
  match (name, children) with
  | _, arg :: [] -> tokenize_rec working_tokenization arg
  | _ -> working_tokenization

and math_tokenizer (working_tokenization : proof_token list list)
    (math_latex : Latex.latex) =
  match (working_tokenization, parse_math math_latex) with
  | [], _ -> []
  | _, [] -> working_tokenization
  | head :: tail, math :: [] -> (MathToken math :: head) :: tail
  | _, math_lines ->
      List.map ~f:(fun math -> [ MathToken math ]) math_lines
      @ working_tokenization

let tokenize (latex : Latex.t) =
  List.filter ~f:(not << List.is_empty) (tokenize_rec [ [] ] latex)
