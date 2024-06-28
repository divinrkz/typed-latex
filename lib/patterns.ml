open Core
open Proof_lex
open Ast
open User
open Fn

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
  | Other
[@@deriving eq, show, sexp, hash, ord]

type elementary_relation = ERelation of relation_type * Math.t * Math.t

let ast_to_elementary_relation_type (ast_rel_t : Math.relation) =
  match ast_rel_t with
  | Math.Le -> Le
  | Math.Leq -> Leq
  | Math.Ge -> Ge
  | Math.Geq -> Geq
  | Math.Eq -> Eq
  | Math.In -> In
  | Math.NotIn -> NotIn
  | Math.Subset -> Subset
  | Math.Superset -> Superset
  | Math.SubsetEq -> SubsetEq
  | Math.SupersetEq -> SupersetEq
  | _ -> Other

let rec extract_elementary_relations (bound : Math.t)
    (tail : (Math.relation * Math.t) list) =
  match tail with
  | (ast_rel_t, right) :: (_, left) :: remaining ->
      ERelation (ast_to_elementary_relation_type ast_rel_t, left, right)
      :: extract_elementary_relations bound remaining
  | (ast_rel_t, right) :: [] -> [ERelation (ast_to_elementary_relation_type ast_rel_t, bound, right)]
  | [] -> []

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
  (* Primary *)
  | Word of string
  | Any of pattern list
  | Sequence of pattern list
  | Optional of pattern
  | Repeat of pattern
  | TypeName of MatchID.t
  | DefContainer of pattern * MatchID.t
  | Relation of relation_type * MatchID.t * MatchID.t
  (* Auxiliary *)
  | OptRepeat of pattern

let def =
  Sequence
    [
      Any [ Word "choose"; Word "consider"; Word "define" ];
      Relation (Eq, MatchID.from_int 1, MatchID.from_int 2);
    ]

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

let box_context (match_id : MatchID.t) (exterior_matches : MatchContainer.t)
    (interior_context : context) =
  match interior_context with
  | tokens, interior_matches ->
      ( tokens,
        MatchContainer.put exterior_matches match_id
          (DefContainerMatch interior_matches) )

let rec extract_type_names (opt : bool) (tokens : proof_token list) =
  match tokens with
  | WordToken word :: remainder ->
      if List.mem ~equal:String.equal Util.non_type_words word then
        if opt then [ ("", tokens) ] else []
      else
        (word, remainder)
        :: ((fun (s, r) -> (word ^ s, r)) |<<: extract_type_names true remainder)
  | _ -> if opt then [ ("", tokens) ] else []

let box_type_name (match_id : MatchID.t) (exterior_matches : MatchContainer.t)
    ((name, tokens) : string * proof_token list) =
  (tokens, MatchContainer.put exterior_matches match_id (TypeNameMatch name))

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
  | TypeName match_id, (tokens, exterior_matches) ->
      let type_name_opts = extract_type_names false tokens in
      box_type_name match_id exterior_matches |<<: type_name_opts
  | DefContainer (p, match_id), (tokens, exterior_matches) ->
      let interior_contexts = match_rec p (tokens, MatchContainer.empty) in
      box_context match_id exterior_matches |<<: interior_contexts
  (* | ( Relation (relation_type, left_id, right_id),
      (MathToken math :: remainder, exterior_matches) ) when ->
      [] *)
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
