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

let to_ast_relation (relation: relation_type) =
  match relation with 
    | Le -> Ast.Math.Le
    | Leq -> Ast.Math.Leq
    | Ge -> Ast.Math.Ge
    | Geq -> Ast.Math.Geq
    | Eq -> Ast.Math.Eq
    | In -> Ast.Math.In
    | NotIn -> Ast.Math.NotIn
    | Subset -> Ast.Math.Subset
    | Superset -> Ast.Math.Superset
    | SubsetEq -> Ast.Math.SubsetEq
    | SupersetEq -> Ast.Math.SupersetEq
  
let rec relation_simplified_eq (a: relation_type) (b: Ast.Math.t) =
  match (a, b) with
    | Le, Ast.Math.Relation (_, ((Ast.Math.Le, _) :: _)) -> true
    | Leq, Ast.Math.Relation (_, ((Ast.Math.Leq, _) :: _)) -> true
    | Ge, Ast.Math.Relation (_, ((Ast.Math.Ge, _) :: _)) -> true
    | Geq, Ast.Math.Relation (_, ((Ast.Math.Geq, _) :: _)) -> true
    | Eq, Ast.Math.Relation (_, ((Ast.Math.Eq, _) :: _)) -> true
    | In, Ast.Math.Relation (_, ((Ast.Math.In, _) :: _)) -> true
    | NotIn, Ast.Math.Relation (_, ((Ast.Math.NotIn, _) :: _)) -> true
    | Subset, Ast.Math.Relation (_, ((Ast.Math.Subset, _) :: _)) -> true
    | Superset, Ast.Math.Relation (_, ((Ast.Math.Superset, _) :: _)) -> true
    | SubsetEq, Ast.Math.Relation (_, ((Ast.Math.SubsetEq, _) :: _)) -> true
    | SupersetEq, Ast.Math.Relation (_, ((Ast.Math.SupersetEq, _) :: _)) -> true
    | relation, Ast.Math.Relation (bound_var, (_ :: rem_bindings)) -> relation_simplified_eq relation (Ast.Math.Relation (bound_var, rem_bindings))

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

let def = Sequence [Any [Word "choose"; Word "consider"; Word "define"]; Relation 0]

(* let test_def = Sequence [Word "Let"; Variable 0; Option (Sequence [Word "represent"; Word "be"]);
 SpecificRelation Leq; Option (Sequence [Word "an"; Word "some"; Word "equal"] Definition 0);];  *)

type result_t = (int, Math.t) Hashtbl.t

let match_with (option_latex: Ast.Latex.t option) (pat: pattern) =
  match option_latex with
  | Some latex ->
      let (mappings: result_t) = Hashtbl.create (module Int) in
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
          let result_t = parse_math (Mathmode x) in
          match result_t with
          | [Variable t] -> (
            Hashtbl.set mappings ~key:id ~data:(Variable t);
            true
          ) 
          | _ -> false
        )
        | ({ pos = _; value = Mathmode x}, Relation id) -> (
          let result_t = parse_math (Mathmode x) in
          match result_t with
          | [Relation _ as rel] -> (
            Hashtbl.set mappings ~key:id ~data:rel;
            true
          )
          | _ -> false
        
        )
        | ({ pos = _; value = Mathmode x}, SpecificRelation (id, relation)) -> (
          let result_t = parse_math (Mathmode x) in
          match result_t with
          | [Relation (ast_relation, _) as rel] when relation_simplified_eq relation ast_relation -> (
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
  | _ -> None
