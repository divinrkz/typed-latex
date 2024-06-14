open Core
(* open Proof_lex *)
open Ast
(* open User *)
open Fn
open Util

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

let rec relation_simplified_eq (relation : relation_type) (ast_rel : Math.t) =
  match (relation, ast_rel) with
  | Le, Math.Relation (_, (Math.Le, _) :: _) -> true
  | Leq, Math.Relation (_, (Math.Leq, _) :: _) -> true
  | Ge, Math.Relation (_, (Math.Ge, _) :: _) -> true
  | Geq, Math.Relation (_, (Math.Geq, _) :: _) -> true
  | Eq, Math.Relation (_, (Math.Eq, _) :: _) -> true
  | In, Math.Relation (_, (Math.In, _) :: _) -> true
  | NotIn, Math.Relation (_, (Math.NotIn, _) :: _) -> true
  | Subset, Math.Relation (_, (Math.Subset, _) :: _) -> true
  | Superset, Math.Relation (_, (Math.Superset, _) :: _) -> true
  | SubsetEq, Math.Relation (_, (Math.SubsetEq, _) :: _) -> true
  | SupersetEq, Math.Relation (_, (Math.SupersetEq, _) :: _) -> true
  | relation, Math.Relation (bound_var, _ :: rem_bindings) ->
      relation_simplified_eq relation (Math.Relation (bound_var, rem_bindings))
  | _ -> false

module MatchID = struct
  module T = struct
    type t = int [@@deriving eq, show, sexp, hash, ord, compare]
  end

  include T
  include Comparable.Make (T)

  let from_int = id
end

type pattern =
  | Word of string
  | Any of pattern list
  | Sequence of pattern list
  | Optional of pattern
  | Repeat of pattern
  | OptRepeat of pattern
  | TypeName of MatchID.t
  | DefContainer of MatchID.t
  | Relation of relation_type * MatchID.t * MatchID.t
[@@deriving eq, show, sexp, hash, ord]

let def =
  Sequence
    [
      Any [ Word "choose"; Word "consider"; Word "define" ];
      Relation (Eq, MatchID.from_int 1, MatchID.from_int 2);
    ]

(* 
let parse_relation str =
  match str with
  | "Mless_than" -> Le
  | "Mless_than_or_equal" -> Leq
  | "Mgreater_than" -> Ge
  | "Mgreater_than_or_equal" -> Geq
  | "Mequal" -> Eq
  | "Mnot_equal" -> NotIn
  | "Mset" -> In
  | _ -> failwith "Unknown relation type"

let rec parse_tokens tokens =
  match tokens with
  | [] -> []
  | token :: rest ->
    if Re.execp regex_word token then
      Word token :: parse_tokens rest
    else if Re.execp regex_type_name token then
      TypeName 1 :: parse_tokens rest 
    else if Re.execp regex_relation token then
      let rel = parse_relation token in
      Relation (rel, 1, 2) :: parse_tokens rest 
    else
      failwith ("Unknown token: " ^ token)

let parse_pattern str =
  let tokens = Re.split (Re.Perl.compile_pat "[ \t\n\r]+") str in
  Sequence (parse_tokens tokens)
 *)

 

 let extract_patterns filename = 
  In_channel.with_file filename ~f:(fun input_c ->
    let line_counter = ref 0 in 
    In_channel.iter_lines input_c ~f:(fun line -> 
      incr line_counter;
      print_string ((string_of_int !line_counter) ^ ": ");
      print_endline line;
      let splits = Util.str_split line ":" in
      match List.hd splits with 
      | Some split -> (
          match Util.regex_matcher split Util.word_regex with
          | Some str -> print_endline str
          | None -> print_endline "No match found"
        )
      | None -> ()
    )
  )


(* let parse_file filename =
  In_channel.with_file filename ~f:(fun input_c ->
    In_channel.iter_lines input_c ~f:(fun line ->
      let pattern = parse_pattern line in
      print_endline (show_pattern pattern)
    )
  ) *)

type proof_token =
  | PunctuationToken of string
  | WordToken of string
  | MathToken of Ast.Math.t
module rec MatchContainer : sig
  type value =
    | DefContainerMatch of t
    | TypeNameMatch of string
    | ExpressionMatch of Math.t

  and t = value MatchID.Map.t

  val empty : t
  val put : t -> MatchID.t -> value -> t
end = struct
  type value =
    | DefContainerMatch of t
    | TypeNameMatch of string
    | ExpressionMatch of Math.t

  and t = value MatchID.Map.t

  let empty = MatchID.Map.empty

  let put (map : t) (k : MatchID.t) (v : value) =
    Core.Map.set map ~key:k ~data:v
end

type context = proof_token list * MatchContainer.t
type nd_context = context list

let rec match_rec (pat : pattern) (context : context) =
  match (pat, context) with
  | Word p_word, (WordToken word :: remainder, matches)
    when String.equal p_word word ->
      [ (remainder, matches) ]
  | Any ps, _ -> flip match_rec context =<<: ps
  | Sequence [], _ -> [ context ]
  | Sequence (p :: ps), _ -> match_rec (Sequence ps) =<<: match_rec p context
  | Optional p, _ -> context :: match_rec p context
  | OptRepeat p, _ -> context :: (match_rec pat =<<: match_rec p context)
  | Repeat p, _ -> match_rec (Sequence [ p; OptRepeat p ]) context
  | _ -> []

(* type result_t = (int, Math.t) Hashtbl.t *)

(* let match_with (pat : pattern) (latex : Latex.t) =
   let (mappings : result_t) = Hashtbl.create (module Int) in
   let rec recurse (pat : pattern) (node : Latex.t) =
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
   if recurse pat latex then Some mappings else None *)
