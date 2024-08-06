open Core
open Patterns
open Util
(* open Spectrum *)


module PatternDef = struct 
  module T = struct 
    type t = {
      id: int;
      line: string;
      pattern: pattern;
    }
    [@@deriving eq, show, sexp, hash, ord, compare]
  end

  include T
  include Comparable.Make (T)

  let constr (id, line, pattern) = { id; line; pattern }

  let get_id t = t.id
  let get_line t = t.line
  let get_pattern t = t.pattern

  (* let to_string t = Simple.printf "@{<red>%s@}\n" "pattern to string:" *)
end  

module PatternExtractor = struct
  type t = {
    mutable patterns: PatternDef.t list;
  }

  let constr = { patterns = [] }

  let add_pattern extractor pattern = 
    extractor.patterns <- List.append extractor.patterns [pattern]

  let get_patterns extractor = extractor.patterns

  (* let find_by_id extractor id = 
    List.find_opt (fun pattern -> PatternDef.get_id pattern = id) extractor.patterns *)

  let curr_match_id = ref 0

  let gen_id () = 
    incr curr_match_id;
    !curr_match_id

  let regex_first_split = "|?[a-zA-Z]+|?"
  let regex_optional = "\\(([^()]+)\\)\\?"
  let relation_regex = "[a-zA-Z]+"

  let parse_relation_type (relation_type: string) = 
    match relation_type with
    | "Mset" -> "Set"
    | "Min" -> "In"
    | "Mgreater_than" -> "StrictGreaterThan"
    | "Mequal" -> "Equality"
    | "Mnot_equal" -> "NotEquality"
    | "Mgreater_than_or_equal" -> "GreaterThan"
    | "Mless_than" -> "StrictLessThan"
    | "Mless_than_or_equal" -> "LessThan"
    | _ -> "Other"

  let parse_relations relations_str =
    let def_container = ref [] in
    let relation_splits = str_split relations_str ' ' in
    let rec parse_relation splits =
      match splits with
      | [] -> !def_container
      | relation :: rest ->
        let parsed_relation = DefContainer (MathPattern (Function (parse_relation_type relation, [Expression (gen_id ()); Expression (gen_id ())], (gen_id ()))), (gen_id ())) in
        def_container := !def_container @ [parsed_relation];
        parse_relation rest
    in
    let relations = parse_relation relation_splits in
      match relations with 
      | _ ->  [Any relations]
  
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

  let extract_patterns extractor filename =
    In_channel.with_file filename ~f:(fun input_c ->
      let line_counter = ref 0 in
      In_channel.iter_lines input_c ~f:(fun line ->
        let seq = ref [] in
        incr line_counter;

        let segment_splits = str_split line ':' in
        (match List.nth segment_splits 0 with
          | None -> print_endline ("Line " ^ string_of_int !line_counter ^ ": No first split found.")
          | Some first_split -> process_first_split first_split seq);
        (match List.nth segment_splits 1 with
          | None -> print_endline ("Line " ^ string_of_int !line_counter ^ ": No second split found.")
          | Some second_split -> 
            let parsed = parse_relations second_split in
            seq := !seq @ parsed);
        (match List.nth segment_splits 2 with
          | None -> print_endline ("Line " ^ string_of_int !line_counter ^ ": No third split found.")
          | Some third_split -> process_first_split third_split seq);
        
        let p = PatternDef.constr (!line_counter, line, (Sequence !seq)) in
        (* extractor.patterns <- p :: extractor.patterns; *)
        add_pattern extractor p;

        (* print_endline (PatternDef.get_line p); 
        print_endline (show_pattern (PatternDef.get_pattern p));  *)
      );
    )
end







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
    ]
let relation_types =
  [
    (
      "Equality",
      "Unequality",
      "GreaterThan",
      "LessThan",
      "StrictGreaterThan",
      "StrictLessThan" 
    );
  ]
*)

(* let def1 =
   Sequence
     [
       Any [Word "choose"; Word "let"; Word "consider"; Word "define"];
       DefContainer (any_known_relation_pattern 1, 0);
       Optional
         (Repeat
            (Sequence
               [
                 Any [ Word "and"; Word "," ];
                 DefContainer (any_known_relation_pattern 1, 0);
               ]));
     ]
      *)

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


