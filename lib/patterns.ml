open Core
open Ast
open User

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
    (* let pattern = User.Sequence [Word "Hello"; Variable 0] in *)

let p1 = Sequence []

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
          Format.printf "Matching %a with %s\n" Latex.pp node (show_pattern pat);
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
        | _ -> Format.printf "Could not match %a with %s\n" Latex.pp node (show_pattern pat); false
      in
      if recurse latex pat then
        Some mappings
      else
        None
  | _ ->
     None
