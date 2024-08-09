open Core
open Patterns
open Util

module PatternDef : sig
  type t
  
  val constr: (int * string * pattern) -> t
  val get_id: t -> int
  val get_line: t -> string
  val get_pattern: t -> pattern
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
  type t = { mutable patterns : PatternDef.t list }

  let constr = { patterns = [] }

  let add_pattern extractor pattern =
    extractor.patterns <- List.append extractor.patterns [ pattern ]

  let get_patterns extractor = extractor.patterns

  let curr_match_id = ref 0

  let gen_id () =
    incr curr_match_id;
    MatchID.from_int !curr_match_id

  let get_id () = 
    MatchID.from_int (gen_id ())

  let parse_relation_type (relation_type : string) =
    match relation_type with
    | "Mset" -> "Set"
    | "Min" -> "In"
    | "Mgreater_than" -> "StrictGreaterThan"
    | "Mequal" -> "Equality"
    | "Mnot_equal" -> "NotEqual"
    | "Mgreater_than_or_equal" -> "GreaterThan"
    | "Mless_than" -> "StrictLessThan"
    | "Mless_than_or_equal" -> "LessThan"
    | "Mvar" -> "Variable"
    | "M" -> "All"
    | _ -> "Other"
  
  let all_relation_types = 
    let relation_type relation = 
      DefContainer ( 
        MathPattern (
          Function (relation, [Expression (get_id ()); Expression (get_id ())], 
            (get_id ())
          )
        ),
        (get_id ())
      )  
    in 
      let relation_types = ["Set"; "In"; "StrictGreaterThan"; "Equality"; "NotEqual";
                            "GreaterThan"; "StrictLessThan"; "LessThan"] 
      in
      List.map ~f:relation_type relation_types
  
let extract_first_split first_split = 
  let words = Util.String.split first_split ' ' in
  let rec parse_word word =
    let word = 
      if Util.String.starts_with word "^" then 
        String.sub ~pos:1 ~len:(String.length word - 1) word
      else word 
    in
    let word = 
      if str_ends_with word "$" then 
        String.sub ~pos:0 ~len:(String.length word - 1) word
      else word 
    in
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
          word 
      else
        word
    in
    if String.contains word '|' then
      let parts = Util.String.split word '|' in
      let rec any_of_list = function
        | [] -> []
        | h :: t -> parse_word h :: any_of_list t
      in
      let any_list = List.filter ~f:(function Word _ -> true | DefContainer _ -> true | Optional _ -> true | _ -> false) (any_of_list parts) in 
      if is_optional then Optional (Any any_list)
      else Any any_list
    else
      let is_valid_char c = Char.is_alphanum c || Char.equal c '_' || Char.equal c ',' in
      let is_valid_word w = String.for_all w ~f:is_valid_char in
      if is_valid_word word &&  String.length word > 0 then
        if is_optional then
          if Char.equal (String.get word 0) 'M' then
            let parsed_relation = parse_relation_type word in 
            let relation_type = 
              (match parsed_relation with 
                | "Variable" -> DefContainer (MathPattern (Expression (get_id ())), (get_id ()))
                | _ -> DefContainer (MathPattern (Function (parsed_relation, 
                [Expression (get_id ()); Expression (get_id ())], (get_id ()))), (get_id ()))
            ) in 
            Optional relation_type
          else if String.equal word "D" then
            let df = DefContainer (MathPattern (Expression (get_id ())), (get_id ())) in 
            Optional (df)
          else Optional (Word word)
        else 
          if Char.equal (String.get word 0) 'M' then
            let parsed_relation = parse_relation_type word in 
            let relation_type = 
              (match parsed_relation with 
                | "Variable" -> DefContainer (MathPattern (Expression (get_id ())), (get_id ()))
                | "All" -> Any all_relation_types
                | _ -> DefContainer (MathPattern (Function (parsed_relation, 
                [Expression (get_id ()); Expression (get_id ())], (get_id ()))), (get_id ()))
            ) in 
            relation_type
          else if String.equal word "D" then 
            DefContainer (MathPattern (Expression (get_id ())), (get_id ()))
          else 
            if String.contains word '_' then 
              Word (Util.String. replace_char_at_index word (String.index_exn word '_') ' ')
            else
              Word word 
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

        line_counter := !line_counter + 1;
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
        ( MathPattern (Function ("StrictGreaterThan", [ Expression (PatternExtractor.get_id ()); Expression (PatternExtractor.get_id ()) ], (PatternExtractor.get_id ()))),
          (PatternExtractor.get_id ()));
    ]



    
