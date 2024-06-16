open Core
open Fn 


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

(** Pretty-printing **)

let string_sep str formatter () = Format.pp_print_string formatter str
let ( << ) = compose

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

(** [regex_matcher str regex] returns a list of all substrings in [str] that match the given [regex].
    @param str The input string to search for matches.
    @param regex The regular expression pattern to match against the input string.
    @return A list of all substrings in the input string that match the regular expression pattern.
    Example:
    {[ 
      let matches = regex_matcher "these are four words" "[a-zA-Z]+"
      (* matches will be ["these"; "are"; "four"; "words"] *)
    ]}
*)
let regex_matcher (str: string) (regex: string) =
  let rec find_all_matches acc pos =
      let regexp = Str.regexp regex in
    try

      let start_pos = Str.search_forward regexp str pos in
      let matched_str = Str.matched_string str in
      find_all_matches (matched_str :: acc) (start_pos + String.length matched_str)
    with
    | _ -> List.rev acc
  in
  find_all_matches [] 0 

(** Tuples **)
module Pair : sig
  type ('a, 'b) t = ('a * 'b)
  val first : ('a, 'b) t -> 'a
  val second : ('a, 'b) t -> 'b
end = struct
  type ('a, 'b) t = ('a * 'b)
  let first (x, _) = x
  let second (_, x) = x
end

module Triple : sig
  type ('a, 'b, 'c) t = ('a * 'b * 'c)
  val first : ('a, 'b, 'c) t -> 'a
  val second : ('a, 'b, 'c) t -> 'b
  val third : ('a, 'b, 'c) t -> 'c
end = struct
  type ('a, 'b, 'c) t = ('a * 'b * 'c)
  let first (x, _, _) = x
  let second (_, x, _) = x
  let third (_, _, x) = x
end

(** Other **)

let non_type_words =
  [
    ".";
    ",";
    ";";
    ":";
    "(";
    ")";
    "–";
    "—";
    "\"";
    "\'";
    "and";
    "or";
    "but";
    "not";
    "nor";
    "yet";
    "only";
    "once";
    "then";
    "now";
    "that";
    "while";
    "if";
    "unless";
    "though";
    "because";
    "since";
    "so";
    "as";
    "whether";
    "neither";
    "accordingly";
    "also";
    "consequently";
    "conversely";
    "finally";
    "furthermore";
    "hence";
    "however";
    "instead";
    "likewise";
    "meanwhile";
    "moreover";
    "nevertheless";
    "next";
    "nonetheless";
    "otherwise";
    "similarly";
    "still";
    "subsequently";
    "therefore";
    "thus";
  ]
