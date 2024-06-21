open Core
open Ast
open User
(* open Latex_parser *)

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

type pattern =
  | Word of string
  | Option of pattern
  | Sequence of pattern list
  | Any of pattern list
  | Variable of int
  | Relation of int
  | SpecificRelation of int * relation_type
  | Definition of int
[@@deriving eq, show, sexp, hash, ord]

let def =
  Sequence [ Any [ Word "choose"; Word "consider"; Word "define" ]; Relation 0 ]

let test_pattern =
  Sequence
    [
      Any [ Word "choose"; Word "consider"; Word "define" ];
      SpecificRelation (0, Ge);
    ]

type result_t = (int, Math.t) Hashtbl.t

let match_with (pat : pattern) (latex : Ast.Latex.t) =
  let (mappings : result_t) = Hashtbl.create (module Int) in
  let rec recurse (pat : pattern) (node : Ast.Latex.t) =
    match (node, pat) with
    | { pos = _; value = Word x }, Word y ->
        String.equal (String.lowercase x) (String.lowercase y)
    | { pos = _; value = _ }, Option p -> recurse p node
    | { pos = _; value = _ }, Sequence [] -> true
    (* ignore whitespace and newlines *)
    | { pos = p; value = Latex ({ pos = _; value = Whitespace } :: tl) }, _ ->
        recurse pat { pos = p; value = Latex tl }
    | { pos = p; value = Latex ({ pos = _; value = Newline } :: tl) }, _ ->
        recurse pat { pos = p; value = Latex tl }
    (* match sequences *)
    | { pos = p; value = Latex (hd :: tl) }, Sequence (fst :: rest) ->
        if
          (* Format.printf "Matching %a with %s\n" Latex.pp node (show_pattern pat); *)
          recurse fst hd
        then recurse (Sequence rest) { pos = p; value = Latex tl }
        else false
    | { pos = _; value = _ }, Any [] -> false
    | { pos = _; value = _ }, Any (hd :: tl) ->
        if recurse hd node then true else recurse (Any tl) node
    | { pos = _; value = Mathmode x }, Variable id -> (
        let result_t = parse_math (Mathmode x) in
        match result_t with
        | [ Variable t ] ->
            Hashtbl.set mappings ~key:id ~data:(Variable t);
            true
        | _ -> false)
    | { pos = _; value = Mathmode x }, Relation id -> (
        let result_t = parse_math (Mathmode x) in
        match result_t with
        | [ (Relation _ as ast_rel) ] ->
            Hashtbl.set mappings ~key:id ~data:ast_rel;
            true
        | _ -> false)
    | { pos = _; value = Mathmode x }, SpecificRelation (id, relation) -> (
        let result_t = parse_math (Mathmode x) in
        match result_t with
        | [ (Relation _ as ast_rel) ]
          when relation_simplified_eq relation ast_rel ->
            Hashtbl.set mappings ~key:id ~data:ast_rel;
            true
        | _ -> false)
    | _ ->
        Format.printf "Could not match %a with %s\n" Latex.pp node
          (show_pattern pat);
        false
  in
  if recurse pat latex then Some mappings else None
