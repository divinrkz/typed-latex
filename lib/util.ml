open Core
open Fn

(** Functions **)
let ( << ) = compose

let ( <<< ) (f : 'a -> 'b) (x : 'a) = f x
let ( >>> ) (x : 'a) (f : 'a -> 'b) = f x
let apply2 f = flip (flip << f)
let defer2 f = flip << flip f

(** Option monad **)

(* Monadic bind *)
let ( >>=? ) (x : 'a option) (f : 'a -> 'b option) = Option.bind x ~f
let ( =<<? ) (f : 'a -> 'b option) (x : 'a option) = Option.bind x ~f

(* Reverse monadic bind *)
let ( =>>? ) (x : 'a) (f : ('a -> 'b option) option) = ( >>> ) x =<<? f
let ( <<=? ) (f : ('a -> 'b option) option) (x : 'a) = ( >>> ) x =<<? f

(* Applicative chain *)
let ( >>*? ) (x : 'a option) (f : ('a -> 'b) option) = Option.apply f x
let ( *<<? ) (f : ('a -> 'b) option) (x : 'a option) = Option.apply f x

(* Functor map *)
let ( >>|? ) (x : 'a option) (f : 'a -> 'b) = Option.map x ~f
let ( |<<? ) (f : 'a -> 'b) (x : 'a option) = Option.map x ~f

(* Reverse functor map *)
let ( |>>? ) (x : 'a) (f : ('a -> 'b) option) = ( >>> ) x |<<? f
let ( <<|? ) (f : ('a -> 'b) option) (x : 'a) = ( >>> ) x |<<? f

(* Monadic chain-bind *)
let ( >>=*? ) (x : 'a option) (f : ('a -> 'b option) option) = ( >>=? ) x =<<? f
let ( *=<<? ) (f : ('a -> 'b option) option) (x : 'a option) = ( >>=? ) x =<<? f

(* Side-effect map *)
let ( <-<? ) (f : 'a -> unit) (x : 'a option) = Option.iter ~f x
let ( >->? ) (x : 'a option) (f : 'a -> unit) = Option.iter ~f x

(** List monad **)

module List : sig
  include module type of Core.List

  val zip_shorter : 'a list -> 'b list -> ('a * 'b) list
  val insert : 'a list -> 'a list -> int -> 'a list
  val replace_slice : 'a list -> 'a list -> int -> int -> 'a t
end = struct
  include Core.List

  let zip_shorter (x : 'a list) (y : 'b list) : ('a * 'b) list =
    match zip_with_remainder x y with out, _ -> out

  let insert (x : 'a list) (insert_list : 'a list) (i : int) =
    let left, right = split_n x i in
    left @ insert_list @ right

  let replace_slice (x : 'a list) (insert_list : 'a list) (i : int)
      (slice_len : int) =
    let left, center_right = split_n x i in
    let right = drop center_right slice_len in
    left @ insert_list @ right
end

(* Monadic bind *)
let ( >>=: ) (x : 'a list) (f : 'a -> 'b list) = List.bind x ~f
let ( =<<: ) (f : 'a -> 'b list) (x : 'a list) = List.bind x ~f

(* Reverse monadic bind *)
let ( =>>: ) (x : 'a) (f : ('a -> 'b list) list) = ( >>> ) x =<<: f
let ( <<=: ) (f : ('a -> 'b list) list) (x : 'a) = ( >>> ) x =<<: f

(* Functor map *)
let ( >>|: ) (x : 'a list) (f : 'a -> 'b) = List.map x ~f
let ( |<<: ) (f : 'a -> 'b) (x : 'a list) = List.map x ~f

(* Reverse functor map *)
let ( |>>: ) (x : 'a) (f : ('a -> 'b) list) = ( >>> ) x |<<: f
let ( <<|: ) (f : ('a -> 'b) list) (x : 'a) = ( >>> ) x |<<: f

(* Applicative chain *)
let ( >>*: ) (x : 'a list) (f : ('a -> 'b) list) = ( >>|: ) x =<<: f
let ( *<<: ) (f : ('a -> 'b) list) (x : 'a list) = ( >>|: ) x =<<: f

(* Monadic chain-bind *)
let ( >>=*: ) (x : 'a list) (f : ('a -> 'b list) list) = ( >>=: ) x =<<: f
let ( *=<<: ) (f : ('a -> 'b list) list) (x : 'a list) = ( >>=: ) x =<<: f

(* Side-effect map *)
let ( <-<: ) (f : 'a -> unit) (x : 'a list) = List.iter ~f x
let ( >->: ) (x : 'a list) (f : 'a -> unit) = List.iter ~f x

(** Result monad **)

module Result : sig
  include module type of Core.Result

  val tell_map : ('a, 'e) result -> ok:('a -> 'b) -> error:('e -> 'b) -> 'b
  val tell : ('a, 'a) result -> 'a
end = struct
  include Core.Result

  let tell_map (r : ('a, 'e) t) ~(ok : 'a -> 'b) ~(error : 'e -> 'b) : 'b =
    match r with Ok x -> ok x | Error e -> error e

  let tell (r : ('a, 'a) t) : 'a = tell_map r ~ok:id ~error:id
end

(* Monadic bind *)
let ( >>=! ) (x : ('a, 'e) result) (f : 'a -> ('b, 'e) result) =
  Result.bind x ~f

let ( =<<! ) (f : 'a -> ('b, 'e) result) (x : ('a, 'e) result) =
  Result.bind x ~f

(* Reverse monadic bind *)
let ( =>>! ) (x : 'a) (f : ('a -> ('b, 'e) result, 'e) result) =
  ( >>> ) x =<<! f

let ( <<=! ) (f : ('a -> ('b, 'e) result, 'e) result) (x : 'a) =
  ( >>> ) x =<<! f

(* Functor map *)
let ( >>|! ) (x : ('a, 'e) result) (f : 'a -> 'b) = Result.map x ~f
let ( |<<! ) (f : 'a -> 'b) (x : ('a, 'e) result) = Result.map x ~f
let ( >>|!! ) (x : ('a, 'e1) result) (f : 'e1 -> 'e2) = Result.map_error x ~f
let ( |<<!! ) (f : 'e1 -> 'e2) (x : ('a, 'e1) result) = Result.map_error x ~f

(* Reverse functor map *)
let ( |>>! ) (x : 'a) (f : ('a -> 'b, 'e) result) = ( >>> ) x |<<! f
let ( <<|! ) (f : ('a -> 'b, 'e) result) (x : 'a) = ( >>> ) x |<<! f
let ( |>>!! ) (x : 'e1) (f : ('a, 'e1 -> 'e2) result) = ( >>> ) x |<<!! f
let ( <<|!! ) (f : ('a, 'e1 -> 'e2) result) (x : 'e1) = ( >>> ) x |<<!! f

(* Applicative chain *)
let ( >>*! ) (x : ('a, 'e) result) (f : ('a -> 'b, 'e) result) =
  ( >>|! ) x =<<! f

let ( *<<! ) (f : ('a -> 'b, 'e) result) (x : ('a, 'e) result) =
  ( >>|! ) x =<<! f

(* Monadic chain-bind *)
let ( >>=*! ) (x : ('a, 'e) result) (f : ('a -> ('b, 'e) result, 'e) result) =
  ( >>=! ) x =<<! f

let ( *=<<! ) (f : ('a -> ('b, 'e) result, 'e) result) (x : ('a, 'e) result) =
  ( >>=! ) x =<<! f

(* Side-effect map *)
let ( <-<! ) (f : 'a -> unit) (x : ('a, 'e) result) = Result.iter ~f x
let ( >->! ) (x : ('a, 'e) result) (f : 'a -> unit) = Result.iter ~f x
let ( <-<!! ) (f : 'e -> unit) (x : ('a, 'e) result) = Result.iter_error ~f x
let ( >->!! ) (x : ('a, 'e) result) (f : 'e -> unit) = Result.iter_error ~f x

(** Functor casts **)

(* Result -> Result *)

let ( <!<! ) (res : 'a) (x : ('a, 'e0) result) = (fun _ -> res) |<<! x
let ( <!!<!! ) (err : 'e1) (x : ('a, 'e0) result) = (fun _ -> err) |<<!! x

(* Option -> Result *)

let ( <!<? ) (res : 'a) (x : 'e option) =
  Option.value_map ~f:Result.Error.return ~default:(Ok res) x

let ( <!!<? ) (err : 'e) (x : 'a option) =
  Option.value_map ~f:Result.return ~default:(Error err) x

(** Pretty-printing **)

let string_sep str formatter () = Format.pp_print_string formatter str

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
let str_split str sep = String.split str ~on:sep

let str_ends_with str suffix = Stdlib.String.ends_with ~suffix str

(** [read_file_as_str filename] reads the content of the file specified by [filename]
    line by line. The function writes the lines into a buffer and returns a string with 
    each line followed by a newline character.

    @param filename The name of file to be read.
    @return String will all lines from file
*)
let read_file_as_str (filename : string) =
  let buffer = Buffer.create 4096 in
  In_channel.with_file filename ~f:(fun input_c ->
      In_channel.iter_lines input_c ~f:(fun line ->
          Buffer.add_string buffer line));
  Buffer.contents buffer

let regex_first_matcher (str : string) (regex : string) =
  try
    let regexp = Str.regexp regex in
    let _ = Str.search_forward regexp str 0 in
    Str.matched_string str
  with _ -> ""

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
let regex_matcher (str : string) (regex : string) =
  let rec find_all_matches acc pos =
    let regexp = Str.regexp regex in
    try
      let start_pos = Str.search_forward regexp str pos in
      let matched_str = Str.matched_string str in
      find_all_matches (matched_str :: acc)
        (start_pos + String.length matched_str)
    with _ -> List.rev acc
  in
  find_all_matches [] 0

(** Tuples **)
module Pair : sig
  type ('a, 'b) t = 'a * 'b

  val first : ('a, 'b) t -> 'a
  val second : ('a, 'b) t -> 'b
  val build : 'a -> 'b -> ('a, 'b) t
end = struct
  type ('a, 'b) t = 'a * 'b

  let first (x, _) = x
  let second (_, x) = x
  let build x y = (x, y)
end

module Triple : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  val first : ('a, 'b, 'c) t -> 'a
  val second : ('a, 'b, 'c) t -> 'b
  val third : ('a, 'b, 'c) t -> 'c
  val build : 'a -> 'b -> 'c -> ('a, 'b, 'c) t
  val cons : 'a -> 'b * 'c -> ('a, 'b, 'c) t
  val snoc : 'a * 'b -> 'c -> ('a, 'b, 'c) t
end = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let first (x, _, _) = x
  let second (_, x, _) = x
  let third (_, _, x) = x
  let build x y z = (x, y, z)
  let cons x (y, z) = (x, y, z)
  let snoc (x, y) z = (x, y, z)
end

(** Strings **)

module String = struct
  include String

  let non_stupid_slice (str : t) (start : int) (stop : int) =
    let len = length str in
    let m_start, m_stop = (start % len, stop % len) in
    if Int.equal m_start m_stop then make 0 (Char.unsafe_of_int 0)
    else slice str m_start m_stop

  let opt_get (str : t) (i : int) : char option =
    if Int.( >= ) i (length str) then None else Some (get str i)
end

(** Other **)

let word_sep_chars = [ ' '; '\n' ]
let word_split_chars = [ '.'; ','; ';'; '/'; '('; ')' ]
let sentence_split_words = [ "."; ";" ]

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
