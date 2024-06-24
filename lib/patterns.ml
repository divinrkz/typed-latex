open Core
open Ast
open User
(* open Re.Perl *)

type relation_type =
  | Le
  | Leq
  | Ge
  | Geq
  | Eq
  | In
  | NotIn
  | Subset
  | Superset
  | SubsetEq
  | SupersetEq
[@@deriving eq, show, sexp, hash, ord]

let rec relation_simplified_eq (relation : relation_type) (ast_rel : Ast.Math.t)
    =
  match (relation, ast_rel) with
  | Le, Ast.Math.Relation (_, (Ast.Math.Le, _) :: _) -> true
  | Leq, Ast.Math.Relation (_, (Ast.Math.Leq, _) :: _) -> true
  | Ge, Ast.Math.Relation (_, (Ast.Math.Ge, _) :: _) -> true
  | Geq, Ast.Math.Relation (_, (Ast.Math.Geq, _) :: _) -> true
  | Eq, Ast.Math.Relation (_, (Ast.Math.Eq, _) :: _) -> true
  | In, Ast.Math.Relation (_, (Ast.Math.In, _) :: _) -> true
  | NotIn, Ast.Math.Relation (_, (Ast.Math.NotIn, _) :: _) -> true
  | Subset, Ast.Math.Relation (_, (Ast.Math.Subset, _) :: _) -> true
  | Superset, Ast.Math.Relation (_, (Ast.Math.Superset, _) :: _) -> true
  | SubsetEq, Ast.Math.Relation (_, (Ast.Math.SubsetEq, _) :: _) -> true
  | SupersetEq, Ast.Math.Relation (_, (Ast.Math.SupersetEq, _) :: _) -> true
  | relation, Ast.Math.Relation (bound_var, _ :: rem_bindings) ->
      relation_simplified_eq relation
        (Ast.Math.Relation (bound_var, rem_bindings))
  | _ -> false

type id = int [@@deriving eq, show, sexp, hash, ord]

type pattern =
  | Word of string
  | Any of pattern list
  | Sequence of pattern list
  | Optional of pattern
  | Repeat of pattern
  | TypeName of id
  | DefContainer of id
  | Relation of relation_type * id * id
[@@deriving eq, show, sexp, hash, ord]

let def =
  Sequence
    [
      Any [ Word "choose"; Word "consider"; Word "define" ]; Relation (Eq, 1, 2);
    ]

type proof_token =
  | PunctuationToken of string
  | WordToken of string
  | MathToken of Ast.Math.t

let rec tokenize_rec (working_tokenization : proof_token list list)
    (latex : Latex.t) =
  match working_tokenization with
  | head :: tail -> (
      match User.unwrap_node latex with
      | Latex.Word word -> (WordToken word :: head) :: tail
      | Latex.Latex children ->
          List.fold ~f:tokenize_rec ~init:working_tokenization children
      | Latex.Environment (name, args, children) ->
          environment_tokenizer working_tokenization name args children
      | Latex.Command (name, children) ->
          command_tokenizer working_tokenization name children
      | Latex.Mathmode _ as math_latex ->
          math_tokenizer working_tokenization math_latex
      | _ -> working_tokenization)
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
  |[], _ -> []
  | _, [] -> working_tokenization
  | head :: tail, math :: [] -> (MathToken math :: head) :: tail
  | _, math_lines -> (List.map ~f:(fun math -> [MathToken math]) math_lines) @ working_tokenization

type result_t = (int, Math.t) Hashtbl.t

let match_with (pat : pattern) (latex : Ast.Latex.t) =
  let (mappings : result_t) = Hashtbl.create (module Int) in
  let rec recurse (pat : pattern) (node : Ast.Latex.t) =
    match (pat, node) with
    | Word y, { pos = _; value = Word x } ->
        String.equal (String.lowercase x) (String.lowercase y)
    | Option p, { pos = _; value = _ } -> recurse p node
    (* ignore whitespace and newlines *)
    | _, { pos = p; value = Latex ({ pos = _; value = Whitespace } :: tl) } ->
        recurse pat { pos = p; value = Latex tl }
    | _, { pos = p; value = Latex ({ pos = _; value = Newline } :: tl) } ->
        recurse pat { pos = p; value = Latex tl }
    (* match sequences *)
    | Sequence [], { pos = _; value = _ } -> true
    | Sequence (fst :: rest), { pos = p; value = Latex (hd :: tl) } ->
        if
          (* Format.printf "Matching %a with %s\n" Latex.pp node (show_pattern pat); *)
          recurse fst hd
        then recurse (Sequence rest) { pos = p; value = Latex tl }
        else false
    | Any [], { pos = _; value = _ } -> false
    | Any (hd :: tl), { pos = _; value = _ } ->
        if recurse hd node then true else recurse (Any tl) node
    | Variable id, { pos = _; value = Mathmode x } -> (
        let result_t = parse_math (Mathmode x) in
        match result_t with
        | [ Variable t ] ->
            Hashtbl.set mappings ~key:id ~data:(Variable t);
            true
        | _ -> false)
    | Relation id, { pos = _; value = Mathmode x } -> (
        let result_t = parse_math (Mathmode x) in
        match result_t with
        | [ (Relation _ as ast_rel) ] ->
            Hashtbl.set mappings ~key:id ~data:ast_rel;
            true
        | _ -> false)
    | SpecificRelation (id, relation), { pos = _; value = Mathmode x } -> (
        let result_t = parse_math (Mathmode x) in
        match result_t with
        | [ (Relation _ as ast_rel) ]
          when relation_simplified_eq relation ast_rel ->
            Hashtbl.set mappings ~key:id ~data:ast_rel;
            true
        | _ -> false)
    (* | _, { pos = _; value = Environment (_, _, )} *)
    | _ ->
        Format.printf "Could not match %a with %s\n" Latex.pp node
          (show_pattern pat);
        false
  in
  if recurse pat latex then Some mappings else None
