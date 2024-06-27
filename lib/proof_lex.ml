open Core
open Ast
open User

type proof_token =
  | PunctuationToken of string
  | WordToken of string
  | MathToken of Math.t

let tokenize_word (word : string) =
  match word with
  | "." | "," | ";" | "-" | "(" | ")" | "–" | "—" -> PunctuationToken word
  | _ -> WordToken word

let rec tokenize_rec (working_tokenization : proof_token list list)
    (latex : Latex.t) =
  match working_tokenization with
  | head :: tail -> (
      match User.unwrap_node latex with
      | Latex.Word word -> (tokenize_word word :: head) :: tail
      | Latex.Latex children ->
          List.fold ~f:tokenize_rec ~init:working_tokenization children
      | Latex.Environment (name, args, children) ->
          environment_tokenizer working_tokenization name args children
      | Latex.Command (name, children) ->
          command_tokenizer working_tokenization name children
      | Latex.Mathmode _ as math_latex ->
          math_tokenizer working_tokenization math_latex
      | Latex.Multiline _ as math_latex ->
          math_tokenizer working_tokenization math_latex
      | Latex.Newline -> working_tokenization
      | Latex.Whitespace -> working_tokenization)
  | [] -> []

and environment_tokenizer (working_tokenization : proof_token list list)
    (name : string) (args : Latex.t list) (children : Latex.t list) =
  match (name, args, children) with
  | _, _, children ->
      ([] :: List.concat_no_order (List.map ~f:(tokenize_rec [ [] ]) children))
      @ working_tokenization

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

let tokenize (latex : Latex.t) = tokenize_rec [ [] ] latex
