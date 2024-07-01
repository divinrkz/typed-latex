open Core
(* open Proof_lex *)
open Ast
(* open User *)
open Fn
open Tree_print
open Ast_print

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

  let from_int : int -> t = id
  let to_string (match_id : t) : string = "<" ^ string_of_int match_id ^ ">"
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
      Any [ Word "choose"; Word "let"; Word "consider"; Word "define" ];
      any_known_relation_pattern 1;
    ]

(* 
 v x
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

 let get_nth (lst: string list) idx = match List.nth lst idx with 
 | Some elem -> elem
 | None -> ""

let parse_relation_type (relation_type: string) = 
   match relation_type with
   | "Mset" -> Subset
   | "Min" -> In
   | "Mgreater_than" -> Ge
   | "Mgreater_than_or_equal" -> Geq
   | "Mless_than" -> Le
   | "Mless_than_or_equal" -> Leq
   | _ -> failwith "Unknown relation type"

let parse_relations relations_str =
  let relation_splits = str_split relations_str " " in 
    let rec parse_relation splits = 
      match splits with
      | [] -> []
      | relation :: rest -> Relation (parse_relation_type relation, 1, 2) :: parse_relation rest 
    in
    parse_relation relation_splits


let parse_typenames str = 
  let typename_splits = str_split str " " in 
    let rec parse_typename splits = 
        match splits with 
        | [] -> []
        | str :: rest -> Word str :: parse_typename rest
    in 
    parse_typename typename_splits
      
let parse_patterns filename = 
  let seq = ref (Sequence []) in 
  In_channel.with_file filename ~f:(fun input_c ->
    let line_counter = ref 0 in 
      In_channel.iter_lines input_c ~f:(fun line -> 
        incr line_counter;
        print_string ((string_of_int !line_counter) ^ ": ");
        print_endline line;
        let splits = Util.str_split line ":" in
          let first_split = get_nth splits 0 in 
            (match Util.regex_matcher first_split Util.word_regex with
               | Some str -> (* sequence @ Word str;*) seq := Sequence [Word str] 
              | None -> print_endline ("Line " ^ string_of_int !line_counter ^ "No match found.")
            ); 
          let second_split = get_nth splits 1 in 
            (let parsed = Any (parse_relations second_split) in 
            (* sequence @ parsed *)
              seq := Sequence (!seq :: [parsed]);
          );
          let third_split = get_nth splits 2 in
            (let parsed =  parse_typenames third_split in 
              (* sequence @ parsed *)
                seq := Sequence (!seq :: parsed);
                print_endline (show_pattern !seq)
            );
    )
  )


(* let def = Sequence [(Patterns.Sequence [(Patterns.Word "let")]);
  (Patterns.Any
     [(Patterns.Relation (Patterns.Subset, 1, 2));
       (Patterns.Relation (Patterns.In, 1, 2));
       (Patterns.Relation (Patterns.Ge, 1, 2));
       (Patterns.Relation (Patterns.Geq, 1, 2));
       (Patterns.Relation (Patterns.Le, 1, 2));
       (Patterns.Relation (Patterns.Leq, 1, 2))])
  ])
   *)


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
    | RelationMatch of elementary_relation

  and t = value MatchID.Map.t

  val empty : t
  val put : t -> MatchID.t -> value -> t
  val compare : t -> t -> int
  val to_string_tree : t -> tree_print_node
  val tree_format : t -> string
end = struct
  type value =
    | DefContainerMatch of t
    | TypeNameMatch of string
    | RelationMatch of elementary_relation

  and t = value MatchID.Map.t

  let empty = MatchID.Map.empty

  let put (map : t) (k : MatchID.t) (v : value) =
    Core.Map.set map ~key:k ~data:v

  let rec recursive_size (map : t) =
    List.sum (module Int) ~f:recursive_value_size (Core.Map.data map)

  and recursive_value_size (v : value) =
    match v with DefContainerMatch sub_map -> recursive_size sub_map | _ -> 1

  let compare (a : t) (b : t) =
    Int.compare (recursive_size a) (recursive_size b)

  let rec to_string_tree (map : t) =
    Branch
      ( Some "MatchContainer",
        ascociation_to_string_tree |<<: Core.Map.to_alist map )

  and ascociation_to_string_tree ((k, v) : MatchID.t * value) =
    Branch (Some ("@" ^ MatchID.to_string k), [ value_to_string_tree v ])

  and value_to_string_tree (v : value) =
    match v with
    | DefContainerMatch sub_map -> to_string_tree sub_map
    | TypeNameMatch type_name -> Leaf type_name
    | RelationMatch (ERelation (rel_type, left, right)) ->
        Branch
          ( Some ("Relation: " ^ show_relation_type rel_type),
            [
              Branch (Some "@left", [ latex_math_to_string_tree left ]);
              Branch (Some "@right", [ latex_math_to_string_tree right ]);
            ] )

  let tree_format = tree_format "| " << to_string_tree
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
    when String.equal p_word (String.lowercase word) ->
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

let compare_context ((a_tokens, a_matches) : context)
    ((b_tokens, b_matches) : context) =
  let match_comparison = MatchContainer.compare a_matches b_matches in
  if match_comparison = 0 then
    Int.compare (List.length b_tokens) (List.length a_tokens)
  else match_comparison

let match_pattern (pat : pattern) (tokenization : proof_token list) =
  let matched_contexts = match_rec pat (tokenization, MatchContainer.empty) in
  List.max_elt ~compare:compare_context matched_contexts
