open Core
open Patterns


type match_id_counters = {
  type_name: int ref;
  def_container: int ref;
  relation: int ref;
  expression: int ref;
}

let id_counters: match_id_counters = {
  type_name = ref 0;
  def_container = ref 0;
  relation = ref 0;
  expression = ref 0;
}


(** Generate a new ID for a given pattern variant *)
let generate_match_id (pattern_case: string) = 
    match pattern_case with 
    | "TypeName" -> incr id_counters.type_name;   
                    !(id_counters.type_name) 
    | "DefContainer" -> incr id_counters.def_container;
                        !(id_counters.def_container) 
    | "Relation" -> incr id_counters.relation;
                    !(id_counters.relation) 
    | "Expression" -> incr id_counters.expression;
                      !(id_counters.expression) 
    | _ -> failwith "Invalid pattern case"


(* Regex patterns *)
let regex_first_split = "|?[a-zA-Z]+|?"
let regex_optional = "\\(([^()]+)\\)\\?"
let relation_regex = "[a-zA-Z]+"

let list_relation_types_pattern (match_id : MatchID.t)
    (allowed_relation_types : relation_type list) =
  Any
    ((fun rel_type -> Relation (rel_type, match_id)) |<<: allowed_relation_types)

let any_known_relation_pattern (match_id : MatchID.t) =
  list_relation_types_pattern match_id all_known_relation_types

let any_relation_pattern (match_id : MatchID.t) =
  list_relation_types_pattern match_id all_relation_types

let parse_relation_type (relation_type: string) = 
   match relation_type with
   | "Mset" -> Subset 
   | "Min" -> In
   | "Mgreater_than" -> Ge
   | "Mequal" -> Eq
   | "Mnot_equal" -> NotEq
   | "Mgreater_than_or_equal" -> Geq
   | "Mless_than" -> Le
   | "Mless_than_or_equal" -> Leq
   | _ -> Other


(** 
  [parse_relations relations_str] processes the [relations_str] by splitting it into relations, 
  and parsing each relation to pattern types

  @param relations_str The string to be processed. 
*)       
let parse_relations relations_str =
  let def_container = ref [] in
  let relation_splits = str_split relations_str ' ' in
  let rec parse_relation splits =
    match splits with
    | [] -> !def_container
    | relation :: rest ->
      let parsed_relation = Relation (parse_relation_type relation, generate_match_id "Relation") in
      def_container := !def_container @ [parsed_relation];
      parse_relation rest
  in
  let relations = parse_relation relation_splits in
    match relations with 
    | [_] ->  [DefContainer (Expression (generate_match_id "Expression"), generate_match_id "DefContainer")]
    | _ ->  [DefContainer (Any relations, generate_match_id "DefContainer")]

(** 
  [parse_words relations_str seq] processes the [relations_str] by splitting it into words, 
  matching them against a regex, and updating the [seq] reference with the results.

  @param relations_str The string to be processed.
  @param seq A reference to the sequence being constructed.
*)     
let parse_words words_str seq =
  let word_splits = str_split words_str ' ' in
    let rec process_words words acc =
      match words with
      | [] -> acc
      | [str] -> acc @ [Word str]
      | str1 :: str2 :: rest ->
          let stripped_str1 = if str_ends_with str1 "|" then String.sub str1 ~pos:0 ~len:(String.length str1 - 1) else str1 in
          let stripped_str2 = if str_ends_with str2 "|" then String.sub str2 ~pos:0 ~len:(String.length str2 - 1) else str2 in
          if str_ends_with str1 "|" || str_ends_with str2 "|" then
            acc @ [Any [Word stripped_str1; Word stripped_str2]] @ process_words rest [] 
          else
            acc @ [Word str1] @ process_words (str2 :: rest) []
    in
      let rec parse_word splits acc =
        match splits with
        | [] -> acc
        | word :: rest ->
          let optional_matches = Util.regex_matcher word regex_optional in
          let new_acc =
            match optional_matches with
            | [opt] -> 
              let stripped_opt = Util.regex_first_matcher opt regex_first_split in 
                acc @ [Optional (Word stripped_opt)]
            | _ -> 
              let matched_lst = Util.regex_matcher word regex_first_split in
              match matched_lst with
              | [] -> acc
              | [str] -> acc @ [Word str]
              | _ -> process_words matched_lst acc
          in
          parse_word rest new_acc
      in
      seq := parse_word word_splits !seq
    

(** 
  [process_first_split first_split seq] processes the [first_split] of a line by calling [parse_words].

  @param first_split The first segment of the line to be processed.
  @param seq A reference to the sequence being constructed.
*)
let process_first_split first_split seq =
  parse_words first_split seq
  
      
(** 
  [parse_patterns filename] reads lines from a file, splits each line into segments, and processes the segments
  to construct a sequence of patterns.

  @param filename The name of the file to read patterns from.
*)
let parse_patterns filename =
  In_channel.with_file filename ~f:(fun input_c ->
    let line_counter = ref 0 in
    In_channel.iter_lines input_c ~f:(fun line ->
    let seq = ref [] in
      incr line_counter;
      print_string ("\nLine [" ^ (string_of_int !line_counter) ^ "]: ");
      print_endline ("\"" ^ line ^ "\"");
      let segment_splits = str_split line ':' in
        match List.nth segment_splits 0 with
          | None -> print_endline ("Line " ^ string_of_int !line_counter ^ ": No first split found.")
          | Some first_split -> process_first_split first_split seq;
        match List.nth segment_splits 1 with
          | None -> print_endline ("Line " ^ string_of_int !line_counter ^ ": No second split found.")
          | Some second_split ->
            let parsed = parse_relations second_split in
              seq := !seq @ parsed;
        match List.nth segment_splits 2 with
        | None -> print_endline ("Line " ^ string_of_int !line_counter ^ ": No first split found.")
        | Some third_split -> process_first_split third_split seq;
      
        print_endline ("Extracted pattern: " ^ show_pattern (Sequence !seq))
    );
  )


let def1 =
  Sequence
    [
      Any [ Word "choose"; Word "let"; Word "consider"; Word "define" ];
      DefContainer (any_known_relation_pattern 1, 0);
      Optional
        (Repeat
           (Sequence
              [
                Any [ Word "and"; Word "," ];
                DefContainer (any_known_relation_pattern 1, 0);
              ]));
    ]
let relation_types =
  [
    ( "Equality",
      "Unequality",
      "GreaterThan",
      "LessThan",
      "StrictGreaterThan",
      "StrictLessThan" );
  ]

(* let def1 =
   Sequence
     [
       Any [ Word "choose"; Word "let"; Word "consider"; Word "define" ];
       DefContainer (any_known_relation_pattern 1, 0);
       Optional
         (Repeat
            (Sequence
               [
                 Any [ Word "and"; Word "," ];
                 DefContainer (any_known_relation_pattern 1, 0);
               ]));
     ] *)

let def =
  Sequence
    [
      Optional (Word "also");
      Optional (Word ",");
      Optional (Sequence [ Word "we"; Word "could" ]);
      Any [ Word "suppose"; Word "say"; Word "imagine"; Word "let" ];
      DefContainer
        ( MathPattern (Function ("StrictGreaterThan", [ Expression 2; Expression 3 ], 1)),
          0 );
    ]
