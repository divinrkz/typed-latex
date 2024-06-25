open Core
open Patterns
open Util

(* Regex patterns *)
let regex_first_split = "|?[a-zA-Z]+|?"
let regex_optional_pattern = "(([^()]*))"
let relation_regex = "[a-zA-Z]+"

let list_relation_types_pattern (match_id : MatchID.t)
    (allowed_relation_types : relation_type list) =
  Any
    ((fun rel_type -> Relation (rel_type, match_id)) |<<: allowed_relation_types)

let any_known_relation_pattern (match_id : MatchID.t) =
  list_relation_types_pattern match_id all_known_relation_types

let any_relation_pattern (match_id : MatchID.t) =
  list_relation_types_pattern match_id all_relation_types

 let get_nth (lst: string list) idx = match List.nth lst idx with 
 | Some elem -> elem
 | None -> ""

let parse_relation_type (relation_type: string) = 
   match relation_type with
   | "Mset" -> In
   | "Min" -> In
   | "Mgreater_than" -> Ge
   | "Mgreater_than_or_equal" -> Geq
   | "Mless_than" -> Le
   | "Mless_than_or_equal" -> Leq
   | _ -> Other

(** 
  [parse_words relations_str line_counter seq] processes the [relations_str] by splitting it into words, 
  matching them against a regex, and updating the [seq] reference with the results.

  @param relations_str The string to be processed.
  @param line_counter The current line number being processed, used for debugging output.
  @param seq A reference to the sequence being constructed.
*)     
let parse_words words_str line_counter seq =
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
          let matched_lst = Util.regex_matcher word regex_first_split in
          let new_acc =
            match matched_lst with
            | [] ->
              print_endline ("Line " ^ string_of_int line_counter ^ ": No match found.");
              acc
            | [str] -> acc @ [Word str]
            | _ -> process_words matched_lst acc
          in
          parse_word rest new_acc
      in
      seq := parse_word word_splits !seq
  
(* Function to process the first split *)
let process_first_split first_split line_counter seq =
  parse_words first_split line_counter seq
  
    
let parse_relations relations_str =
  let relation_splits = str_split relations_str ' ' in 
    let rec parse_relation splits = 
      match splits with
      | [] -> []
      | relation :: rest -> Relation (parse_relation_type relation, 1) :: parse_relation rest 
    in
    parse_relation relation_splits

let parse_typenames str = 
  let typename_splits = str_split str ' ' in 
    let rec parse_typename splits = 
        match splits with 
        | [] -> []
        | str :: rest -> Word str :: parse_typename rest
    in 
    parse_typename typename_splits
     
      
(** 
  [parse_patterns filename] reads lines from a file, splits each line into segments, and processes the segments
  to construct a sequence of patterns.

  @param filename The name of the file to read from.
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
      | Some first_split -> process_first_split first_split !line_counter seq;
        match List.nth segment_splits 1 with
        | None -> print_endline ("Line " ^ string_of_int !line_counter ^ ": No second split found.")
        | Some second_split ->
          let parsed = Any (parse_relations second_split) in
          seq := !seq @ [parsed];
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

let def =
  Sequence
    [
      Word "let";
      DefContainer
        ( Sequence
            [
              Expression 1;
              Word "be";
              Optional
                (Any
                   [ Sequence [ Word "of"; Word "type" ]; Word "a"; Word "an" ]);
              TypeName 2;
            ],
          0 );
    ]
