open Core
open Fn 


let string_sep str formatter () = Format.pp_print_string formatter str

let ( << ) = compose

(** Option monad **)

(* Monadic bind *)
let ( >>=? ) (x : 'a option) (f : 'a -> 'b option) = Option.bind x ~f
let ( =<<? ) (f : 'a -> 'b option) (x : 'a option) = Option.bind x ~f

(* Functor map *)
let ( >>|? ) (x : 'a option) (f : 'a -> 'b) = Option.map x ~f
let ( |<<? ) (f : 'a -> 'b) (x : 'a option) = Option.map x ~f

(* Side-effect map *)
let ( <-<? ) (f : 'a -> unit) (x : 'a option) = Option.iter ~f x

(** List monad **)

(* Monadic bind *)
let ( >>=: ) (x : 'a list) (f : 'a -> 'b list) = List.bind x ~f
let ( =<<: ) (f : 'a -> 'b list) (x : 'a list) = List.bind x ~f

(* Functor map *)
let ( >>|: ) (x : 'a list) (f : 'a -> 'b) = List.map x ~f
let ( |<<: ) (f : 'a -> 'b) (x : 'a list) = List.map x ~f

(* Side-effect map *)
let ( <-<: ) (f : 'a -> unit) (x : 'a list) = List.iter ~f x

let pp_hashtbl formatter ~pp_key ~pp_data t =
  let pp_pair formatter (k, d) =
    Format.fprintf formatter "(%a -> %a)" pp_key k pp_data d
  in
  Format.fprintf formatter "[%a]"
    (Format.pp_print_list ~pp_sep:(string_sep ", ") pp_pair)
    (Hashtbl.to_alist t)


(** [str_split str separator] splits the provided string [str] on a provided 
    separator [sep] and returns a list of split strings
    @param str string to split
    @param sep separator string 
    @return splitted list of string on separator
    *)    
let str_split str sep =
  Str.split (Str.regexp sep) str



(** [read_file_as_str filename] reads the content of the file specified by [filename]
    line by line. The function writes the lines into a buffer and returns a string with 
    each line followed by a newline character.

    @param filename The name of file to be read.
    @return String will all lines from file
*)    
let read_file_as_str (filename: string) = 
   let buffer = Buffer.create 4096 in 
    In_channel.with_file filename ~f:(fun input_c ->
      In_channel.iter_lines input_c ~f:(fun line -> 
          Buffer.add_string buffer line;
        )
  );
  Buffer.contents buffer

(* Regex patterns *)
let word_regex = "[a-zA-Z]+"
let relation_regex = "[a-zA-Z]+"
let regex_type_name = Re.Perl.compile_pat "\\$"
let regex_relation = Re.Perl.compile_pat "Mset|Min|Mgreater_than_or_equal|Mgreater_than|Mless_than_or_equal|Mless_than"
let regex_id = Re.Perl.compile_pat "M\\(in\\|var\\)"


let regex_matcher (str: string) (regex: string) =
  try
    let regexp = Str.regexp regex in
    let _ = Str.search_forward regexp str 0 in
    Some (Str.matched_string str)
  with
    Not_found_s _ -> None
