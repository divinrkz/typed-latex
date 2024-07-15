open Core
(* open Ast *)
(* open User *)
open Fn
open Util
open Latex_deserializer

type proof_token = WordToken of string | MathToken of RawMathLatex.t

let rec tokenize_rec (working_tokenization : proof_token list list)
    (latex : RawLatex.t) =
  match latex with
  | RawLatex.Comment _ -> working_tokenization
  | RawLatex.Environment (name, children) ->
      environment_tokenizer working_tokenization name children
  | RawLatex.Latex children ->
      List.fold_right ~f:(flip tokenize_rec) ~init:working_tokenization children
  | RawLatex.Macro (name, args) ->
      macro_tokenizer working_tokenization name args
  | RawLatex.Math value -> math_tokenizer working_tokenization value
  | RawLatex.MultilineMath value ->
      multiline_math_tokenizer working_tokenization value
  | RawLatex.Text value -> text_tokenizer working_tokenization value

and text_tokenizer (working_tokenization : proof_token list list)
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
      word_tokenizer
        (word_tokenizer
           (text_tokenizer working_tokenization right)
           (String.of_char char))
        left
  | None -> word_tokenizer working_tokenization word

and word_tokenizer (working_tokenization : proof_token list list)
    (word : string) =
  if String.equal word "" then working_tokenization
  else if List.mem ~equal:String.equal Util.sentence_split_words word then
    [] :: working_tokenization
  else
    match working_tokenization with
    | head :: tail -> (WordToken word :: head) :: tail
    | [] -> []

and environment_tokenizer (working_tokenization : proof_token list list)
    (name : string) (children : RawLatex.t list) =
  match (name, children) with
  | ("itemize" | "enumerate"), _ ->
      ([] :: List.concat_no_order (List.map ~f:(tokenize_rec [ [] ]) children))
      @ working_tokenization
  | "document", _ ->
      List.fold_right ~f:(flip tokenize_rec) ~init:working_tokenization children
  | _, _ ->
      List.fold_right ~f:(flip tokenize_rec) ~init:working_tokenization children

and macro_tokenizer (working_tokenization : proof_token list list)
    (name : string) (args : RawLatex.t list) =
  match (name, args) with
  | _, arg :: [] -> tokenize_rec working_tokenization arg
  | _ -> working_tokenization

and math_tokenizer (working_tokenization : proof_token list list)
    (value : RawMathLatex.t) =
  match working_tokenization with
  | head :: tail -> (MathToken value :: head) :: tail
  | [] -> []

and multiline_math_tokenizer (working_tokenization : proof_token list list)
    (value : RawMathLatex.t list) =
  let gen_math (math : RawMathLatex.t) = MathToken math in
  match working_tokenization with
  | [] :: tail -> (List.singleton |<<: (gen_math |<<: value)) @ tail
  | head :: tail -> ((gen_math |<<: value) @ head) :: tail
  | [] -> []

let tokenize (latex : RawLatex.t) =
  List.filter ~f:(not << List.is_empty) (tokenize_rec [ [] ] latex)
