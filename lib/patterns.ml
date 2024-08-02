open Core
open Proof_lex
open Fn
open String_tree
open Latex_deserializer
open Util

module MatchID = struct
  module T = struct
    type t = int [@@deriving eq, show, sexp, hash, ord, compare]
  end

  include T
  include Comparable.Make (T)
  
  let from_int : int -> t = id
  let to_string (match_id : t) : string = "<" ^ string_of_int match_id ^ ">"
end

type math_pattern =
  | TerminalSymbol of MatchID.t
  | Function of string * math_pattern list * MatchID.t
  | Expression of MatchID.t
[@@deriving eq, show, sexp, hash, ord]

type pattern =
  (* Primary *)
  | Word of string
  | Any of pattern list
  | Sequence of pattern list
  | Optional of pattern
  | Repeat of pattern
  | TypeName of MatchID.t
  | DefContainer of pattern * MatchID.t
  | MathPattern of math_pattern
  (* Auxiliary *)
  | OptRepeat of pattern
[@@deriving eq, show, sexp, hash, ord]


let rec extract_elementary_relations (bound : Math.t)
  (tail : (Math.relation * Math.t) list) =
  match tail with
  | (ast_rel_t, right) :: (_, left) :: remaining ->
      ERelation (ast_to_elementary_relation_type ast_rel_t, left, right)
      :: extract_elementary_relations bound remaining
  | (ast_rel_t, right) :: [] ->
      [ ERelation (ast_to_elementary_relation_type ast_rel_t, bound, right) ]
  | [] -> []

module rec MatchContainer : sig
  type match_value =
    | DefContainerMatch of t * ProofToken.t list * int
    | TypeNameMatch of string
    | TerminalSymbolMatch of string
    | ExpressionMatch of RawMathLatex.t
    | FunctionMatch of string

and t = match_value list MatchID.Map.t

  val empty : t
  val put : t -> MatchID.t -> match_value -> t
  val compare : t -> t -> int
  val to_string_tree : t -> string_tree
  val tree_format : t -> string
end = struct
  type match_value =
    | DefContainerMatch of t * ProofToken.t list * int
    | TypeNameMatch of string
    | TerminalSymbolMatch of string
    | ExpressionMatch of RawMathLatex.t
    | FunctionMatch of string

and t = match_value list MatchID.Map.t

let empty = MatchID.Map.empty

let put (map : t) (k : MatchID.t) (v : match_value) =
  Core.Map.add_multi map ~key:k ~data:v

let rec recursive_size (map : t) =
  List.sum
    (module Int)
    ~f:recursive_multi_match_value_size (Core.Map.data map)

and recursive_multi_match_value_size (vs : match_value list) =
  List.sum (module Int) ~f:recursive_match_value_size vs

  and recursive_match_value_size (v : match_value) =
    match v with
    | DefContainerMatch (sub_map, _, _) -> recursive_size sub_map
    | _ -> 1

let compare (a : t) (b : t) =
  Int.compare (recursive_size a) (recursive_size b)

let rec to_string_tree (map : t) =
  Branch
    ( Some "MatchContainer",
      ascociation_to_string_tree |<<: Core.Map.to_alist map )

and ascociation_to_string_tree ((k, vs) : MatchID.t * match_value list) =
  Branch (Some ("@" ^ MatchID.to_string k), match_value_to_string_tree |<<: vs)

  and match_value_to_string_tree (v : match_value) =
    match v with
    | DefContainerMatch (sub_map, captured_tokens, start_index) ->
        Branch
          ( Some "DefContainer",
            [
              to_string_tree sub_map;
              Leaf ("Start index: " ^ string_of_int start_index);
              Branch
                ( Some "Captured Tokens",
                  ProofToken.to_string_tree |<<: captured_tokens );
            ] )
    | TypeNameMatch type_name -> Leaf type_name
    | TerminalSymbolMatch symbol_name -> Leaf symbol_name
    | ExpressionMatch expression -> expression
    | FunctionMatch fun_name -> Leaf fun_name

let tree_format = tree_format "| " << to_string_tree
end

type context = ProofToken.t list * MatchContainer.t * int
type nd_context = context list
type math_context = RawMathLatex.t * MatchContainer.t
type nd_math_context = math_context list

let box_context (match_id : MatchID.t) (exterior_matches : MatchContainer.t)
    (interior_context : context) =
  match interior_context with
  | tokens, interior_matches, interior_start_index ->
      ( tokens,
        MatchContainer.put exterior_matches match_id
          (DefContainerMatch (interior_matches, tokens, interior_start_index)),
        interior_start_index )

let rec extract_type_names (opt : bool) (tokens : ProofToken.t list)
    (start_index : int) =
  match tokens with
  | WordToken word :: remainder ->
      if List.mem ~equal:String.equal Util.non_type_words word then
        if opt then [ ("", tokens, start_index) ] else []
      else
        (word, remainder, start_index + 1)
        :: ((fun (s, r, i) -> (word ^ " " ^ s, r, i))
           |<<: extract_type_names true remainder (start_index + 1))
  | _ -> if opt then [ ("", tokens, start_index) ] else []

let box_type_name (match_id : MatchID.t) (exterior_matches : MatchContainer.t)
    ((name, tokens, index) : string * ProofToken.t list * int) =
  ( tokens,
    MatchContainer.put exterior_matches match_id (TypeNameMatch name),
    index )

let rec match_math_rec (pat : math_pattern) (context : math_context) :
    MatchContainer.t list =
  match (pat, context) with
  | TerminalSymbol match_id, (Leaf symbol_name, exterior_matches) ->
      [
        MatchContainer.put exterior_matches match_id
          (TerminalSymbolMatch symbol_name);
      ]
  | Expression match_id, (expression, exterior_matches) ->
      [
        MatchContainer.put exterior_matches match_id
          (ExpressionMatch expression);
      ]
  | ( Function (p_fun_name, [], match_id),
      (Branch (Some fun_name, []), exterior_matches) )
    when String.equal p_fun_name fun_name ->
      [ MatchContainer.put exterior_matches match_id (FunctionMatch fun_name) ]
  | ( Function (p_fun_name, pat :: pat_tail, match_id),
      (Branch (Some fun_name, arg :: arg_tail), exterior_matches) )
    when String.equal p_fun_name fun_name ->
      let interior_matches =
        match_math_rec
          (Function (p_fun_name, pat_tail, match_id))
          (Branch (Some fun_name, arg_tail), exterior_matches)
      in
      match_math_rec pat =<<: (Pair.build arg |<<: interior_matches)
  | _ -> []

let rec match_rec (pat : pattern) (context : context) : context list =
  match (pat, context) with
  | Word p_word, (WordToken word :: remainder, matches, exterior_start_index)
    when String.equal p_word (String.lowercase word) ->
      [ (remainder, matches, exterior_start_index + 1) ]
  | Any ps, _ -> flip match_rec context =<<: ps
  | Sequence [], _ -> [ context ]
  | Sequence (p :: ps), _ -> match_rec (Sequence ps) =<<: match_rec p context
  | Optional p, _ -> context :: match_rec p context
  | OptRepeat p, _ -> context :: (match_rec pat =<<: match_rec p context)
  | Repeat p, _ -> match_rec (Sequence [ p; OptRepeat p ]) context
  | TypeName match_id, (tokens, exterior_matches, exterior_start_index) ->
      let type_name_opts =
        extract_type_names false tokens exterior_start_index
      in
      box_type_name match_id exterior_matches |<<: type_name_opts
  | DefContainer (p, match_id), (tokens, exterior_matches, exterior_start_index)
    ->
      let interior_contexts =
        match_rec p (tokens, MatchContainer.empty, exterior_start_index)
      in
      box_context match_id exterior_matches |<<: interior_contexts
  | MathPattern mp, (MathToken math :: remainder, exterior_matches, start_index)
    ->
      let interior_context = match_math_rec mp (math, exterior_matches) in
      Triple.build remainder |<<: interior_context <<|: start_index + 1
  | _ -> []

let compare_context ((a_tokens, a_matches, a_index) : context)
    ((b_tokens, b_matches, b_index) : context) =
  let match_comparison = MatchContainer.compare a_matches b_matches in
  if match_comparison = 0 then
    let length_comparaison =
      Int.compare (List.length b_tokens) (List.length a_tokens)
    in
    if length_comparaison = 0 then Int.compare b_index a_index else 0
  else match_comparison

let match_pattern (pat : pattern) (tokenization : ProofToken.t list) =
  let matched_contexts =
    match_rec pat (tokenization, MatchContainer.empty, 0)
  in
  List.max_elt ~compare:compare_context matched_contexts
