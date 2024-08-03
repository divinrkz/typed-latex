open Core
open Patterns
open Util


module PatternDef: sig  
  type t


  val constr: (int * string * pattern) -> t
  val get_id: t -> int
  val get_line: t -> string
  val get_pattern: t -> pattern
  (* val tree_format: t -> string *)
end = struct 
    type t = {
      id: int;
      line: string;
      pattern: pattern;
    }
    [@@deriving eq, show, sexp, hash, ord, compare]


  let constr (id, line, pattern) = { id; line; pattern }

  let get_id t = t.id
  let get_line t = t.line
  let get_pattern t = t.pattern
end


module PatternExtractor = struct
  type t = {
    mutable patterns: PatternDef.t list;
  }

  let constr = { patterns = [] }

  let add_pattern extractor pattern = 
    extractor.patterns <- List.append extractor.patterns [pattern]

  let get_patterns extractor = extractor.patterns

  let curr_match_id = ref 0

  let gen_id () = 
    incr curr_match_id;
    !curr_match_id

  let get_match_id () = 
    MatchID.from_int (gen_id ())

  let regex_first_split = "|?[a-zA-Z]+|?"
  let regex_optional = "\\(([^()]+)\\)\\?"
  let relation_regex = "[a-zA-Z]+"

(* Example usage *)

  let parse_relation_type (relation_type: string) = 
    match relation_type with
    | "Mset" -> "Set"
    | "Min" -> "In"
    | "Mgreater_than" -> "StrictGreaterThan"
    | "Mequal" -> "Equality"
    | "Mnot_equal" -> "NotEqual"
    | "Mgreater_than_or_equal" -> "GreaterThan"
    | "Mless_than" -> "StrictLessThan"
    | "Mless_than_or_equal" -> "LessThan"
    | _ -> "Other"

  let parse_relations relations_str =
    let def_container = ref [] in
    let relation_splits = Util.String.split relations_str ' ' in
    let rec parse_relation splits =
      match splits with
      | [] -> !def_container
      | relation :: rest ->
        let parsed_relation = DefContainer (MathPattern (Function (parse_relation_type relation, [Expression (get_match_id ()); Expression (get_match_id ())], (get_match_id ()))), (get_match_id ())) in
        def_container := !def_container @ [parsed_relation];
        parse_relation rest
    in
    let relations = parse_relation relation_splits in
      match relations with 
      | _ ->  [Any relations]
  
  let parse_words words_str seq =
    let word_splits = Util.String.split words_str ' ' in
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
  
let extract_first_split first_split = 
  let words = Util.String.split first_split ' ' in
  let rec parse_word word =
    let has_dollar = str_ends_with word "$" in 
      let word = if has_dollar then 
        String.sub ~pos:0 ~len:(String.length word - 1) word
      else word in
    let is_optional = str_ends_with word "?" in
    let word =
      if is_optional then
        String.sub ~pos:0 ~len:(String.length word - 1) word
      else
        word
    in
    let word =
      if String.contains word '(' && String.contains word ')' then
        let start_pos = String.index_exn word '(' + 1 in
        let end_pos = String.index_exn word ')' in
        if start_pos < end_pos then
          String.sub ~pos:start_pos ~len:(end_pos - start_pos) word
        else
          word  (* Return original word if positions are not valid *)
      else
        word
    in
    if String.contains word '|' then
      let parts = Util.String.split word '|' in
      let rec any_of_list = function
        | [] -> []
        | h :: t -> parse_word h :: any_of_list t
      in
      let any_list = List.filter ~f:(function Word _ -> true | _ -> false) (any_of_list parts) in 
      if is_optional then Optional (Any any_list)
      else Any any_list
    else
      let is_valid_char c = (Char.is_alphanum c) in
      let is_valid_word w = String.for_all w ~f:is_valid_char in
      if is_valid_word word then
        if is_optional then Optional (Word word)
        else Word word
      else 
        Sequence []
  in
  let rec build_sequence = function
    | [] -> []
    | h :: t ->  (
      match parse_word h with 
      | Sequence [] -> build_sequence t 
      | valid_word -> valid_word :: build_sequence t
    )
  in
  (build_sequence words)

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

        let segment_splits = Util.String.split line ':' in 
          segment_splits >->: (fun split -> 
            let parsed = extract_first_split split in
            seq := !seq @ parsed
          );
        let extracted = PatternDef.constr (!line_counter, line, (Sequence !seq)) in
          add_pattern extractor extracted;
      );
    )
end



let def =
  Sequence
    [
      Optional (Word "also");
      Optional (Word ",");
      Optional (Sequence [ Word "we"; Word "could" ]);
      Any [ Word "suppose"; Word "say"; Word "imagine"; Word "let" ];
      DefContainer
        ( MathPattern (Function ("StrictGreaterThan", [ Expression (PatternExtractor.get_match_id ()); Expression (PatternExtractor.get_match_id ()) ], (PatternExtractor.get_match_id ()))),
          (PatternExtractor.get_match_id ()));
    ]



