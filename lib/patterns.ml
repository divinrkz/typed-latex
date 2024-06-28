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

let all_known_relation_types =
  [ Le; Leq; Ge; Geq; Eq; In; NotIn; Subset; Superset; SubsetEq; SupersetEq ]

let all_relation_types = Other :: all_known_relation_types

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
  | (ast_rel_t, right) :: [] ->
      [ ERelation (ast_to_elementary_relation_type ast_rel_t, bound, right) ]
  | [] -> []

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
  | Relation of relation_type * MatchID.t
  (* Auxiliary *)
  | OptRepeat of pattern

let list_relation_types_pattern (match_id : MatchID.t)
    (allowed_relation_types : relation_type list) =
  Any
    ((fun rel_type -> Relation (rel_type, match_id)) |<<: allowed_relation_types)

let any_known_relation_pattern (match_id : MatchID.t) =
  list_relation_types_pattern match_id all_known_relation_types

let any_relation_pattern (match_id : MatchID.t) =
  list_relation_types_pattern match_id all_relation_types

let def =
  Sequence
    [
      Any [ Word "choose"; Word "consider"; Word "define" ];
      Relation (Eq, MatchID.from_int 1);
    ]

module rec MatchContainer : sig
  type value =
    | DefContainerMatch of t
    | TypeNameMatch of string
    | RelationMatch of elementary_relation

  and t = value MatchID.Map.t

  val empty : t
  val put : t -> MatchID.t -> value -> t
end = struct
  type value =
    | DefContainerMatch of t
    | TypeNameMatch of string
    | RelationMatch of elementary_relation

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

let rec find_elementary_relation (rel_type : relation_type)
    (e_relations : elementary_relation list) =
  match e_relations with
  | (ERelation (found_type, _, _) as out_rel) :: _
    when equal_relation_type found_type rel_type ->
      [ out_rel ]
  | _ :: remainder -> find_elementary_relation rel_type remainder
  | [] -> []

let box_relation (match_id : MatchID.t) (tokens : proof_token list)
    (exterior_matches : MatchContainer.t) (e_relation : elementary_relation) =
  ( tokens,
    MatchContainer.put exterior_matches match_id (RelationMatch e_relation) )

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
  | ( Relation (relation_type, match_id),
      ( MathToken (Math.Relation (ast_head, ast_relations)) :: remainder,
        exterior_matches ) ) ->
      let e_relations = extract_elementary_relations ast_head ast_relations in
      box_relation match_id remainder exterior_matches
      |<<: find_elementary_relation relation_type e_relations
  | _ -> []
