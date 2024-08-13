open Core
open Patterns
open Util
(* open Spectrum *)


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

  (* let rec to_string_tree (pattern : t) =
    match latex with
    | Latex children -> Branch (Some "Latex", to_string_tree |<<: children)
    | Environment (name, children) ->
        Branch (Some ("Environment: " ^ name), to_string_tree |<<: children)
    | Macro (name, args) ->
        Branch (Some ("Macro: " ^ name), to_string_tree |<<: args)
    | Text text -> Leaf ("Text: \"" ^ escape_string text ^ "\"")
    | Comment comment -> Leaf ("Comment: " ^ comment)
    | Math math_tree -> Branch (Some "Math", [ math_tree ])
    | MultilineMath math_trees -> Branch (Some "Multiline Math", math_trees) *)

  (* let tree_format = tree_format "| " << to_string_tree *)
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


  (* Helper functions *)
  let parse_word s = Word s

  let parse_any patterns = Any patterns

  let parse_optional pattern = Optional pattern

  let parse_sequence patterns = Sequence patterns

  let parse_def_container pattern match_id = DefContainer (pattern, match_id)

let parse_math_pattern s =
  if Util.String.starts_with s "M" then
    (* Extract the function name and arguments *)
    (* MathPattern (Function (func_name, [Expression 1, Expression 2], 1)) *)
    MathPattern (Expression 1)
  else
    failwith "Invalid math pattern"

(* Core parsing function *)
let rec parsing_pattern s =
  (* This is where we break down the string and handle each case *)

  (* Case: MathPattern, starting with 'M' *)
  if String.length s > 1 && Util.String.starts_with s "M" then
    parse_def_container (parse_math_pattern s) 1
  (* Case: 'Any' pattern with alternatives separated by | *)
  else if String.contains s '|' then
    let alternatives = Util.String.split s '|' in
    parse_any (List.map ~f:parsing_pattern alternatives)
  (* Case: Optional pattern ending with ? *)
  else if String.length s > 1 && Util.String.ends_with s "?" then
    let base_pattern = String.sub s ~pos:0 ~len:((String.length s) - 1) in
    parse_optional (parsing_pattern base_pattern)
  (* Case: Sequence patterns separated by spaces or special symbols like ",", ":$" etc. *)
  else if String.contains s ' ' || String.contains s ':' then
    let parts = Util.String.split s ' ' in
    parse_sequence (List.map ~f:parsing_pattern parts)
  (* Case: Simple word *)
  else
    parse_word s

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
        let parsed_relation = DefContainer (MathPattern (Function (parse_relation_type relation, [Expression (gen_id ()); Expression (gen_id ())], (gen_id ()))), (gen_id ())) in
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
        String.sub ~pos:start_pos ~len:(end_pos - start_pos) word
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

 (* let extract_first_split first_split = 
   let word_splits = Util.String.split first_split ' ' in
   let rec parse_str word = 
      if str_ends_with word "|" then 
        Optional (parse_str (String.sub ~pos:0 ~len:(String.length word - 1) word))
    else if String.contains word '(' && String.contains word ')' then 
        let start_pos = String.index_exn word '(' + 1 in
        let end_pos = String.index_exn word ')' in
        let stripped = String.sub ~pos:start_pos ~len:(end_pos - start_pos) word in 
        Word stripped
    else if String.contains word '|' then 
        let parts = Util.String.split word '|' in
        let rec any_of_list lst = match lst with 
        | [] -> []
        | h :: t -> parse_str h :: any_of_list t
    in Any (any_of_list parts)
  else Word word
in 
let rec build_seq lst = match lst with 
  | [] ->  []
  | h::t -> parse_str h :: build_seq t
in 
Sequence (build_seq word_splits) *)

  (** 
    [process_first_split first_split seq] processes the [first_split] of a line by calling [parse_words].

    @param first_split The first segment of the line to be processed.
    @param seq A reference to the sequence being constructed.
  *)
  let process_first_split first_split seq =
    parse_words first_split seq 

  (* Parse each part into the corresponding pattern *)
(* let rec parse_part part =
  let len = String.length part in
  if len = 0 then None
  else if Util.String.get_at part 0 = '(' && String.get part (len - 1) = ')' then
    Some (Optional (Word (String.sub part 1 (len - 2))))
  else if part.[len - 1] = '?' then
    Some (Optional (Word (String.sub part 0 (len - 1))))
  else if String.contains part ' ' then
    let sub_parts = split_outside_parentheses part in
    let patterns = List.filter_map parse_part sub_parts in
    Some (Sequence patterns)
  else
    Some (Word part) *)

(* let parse_parts line =
  let segments = String.split_on_char ':' line in
  List.iter (fun segment ->
    print_endline ("Segment: " ^ segment);
    let parts = split_outside_parentheses segment in
    List.iter (fun part ->
      print_endline ("Part: " ^ part);
      match parse_part part with
      | Some pattern -> (* Handle the pattern as needed *)
        (* For now, just printing the pattern type for debugging *)
        begin match pattern with
        | Word s -> print_endline ("Parsed as Word: " ^ s)
        | Optional (Word s) -> print_endline ("Parsed as Optional Word: " ^ s)
        | Sequence _ -> print_endline "Parsed as Sequence"
        | _ -> ()
        end
      | None -> print_endline "No pattern found"
    ) parts
  ) segments *)



  let extract_patterns extractor filename =
    In_channel.with_file filename ~f:(fun input_c ->
      let line_counter = ref 0 in
      In_channel.iter_lines input_c ~f:(fun line ->
        (* let extracted = PatternDef.constr (!line_counter, line, (Sequence [])) in
        add_pattern extractor extracted;
        parse_parts line *)
        let seq = ref [] in
        incr line_counter;

        let segment_splits = Util.String.split line ':' in
        (match List.nth segment_splits 0 with
          | None -> print_endline ("Line " ^ string_of_int !line_counter ^ ": No first split found.")
          | Some first_split -> let parsed = extract_first_split first_split in
          seq := !seq @ parsed);
        (match List.nth segment_splits 1 with
          | None -> print_endline ("Line " ^ string_of_int !line_counter ^ ": No second split found.")
          | Some second_split -> 
            let parsed = parse_relations second_split in
            seq := !seq @ parsed);
        (match List.nth segment_splits 2 with
          | None -> print_endline ("Line " ^ string_of_int !line_counter ^ ": No third split found.")
          | Some third_split -> process_first_split third_split seq);
        
        let extracted = PatternDef.constr (!line_counter, line, (Sequence !seq)) in
          add_pattern extractor extracted;
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



