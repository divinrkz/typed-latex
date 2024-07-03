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
let ( >->? ) (x : 'a option) (f : 'a -> unit) = Option.iter ~f x

(** List monad **)

(* Monadic bind *)
let ( >>=: ) (x : 'a list) (f : 'a -> 'b list) = List.bind x ~f
let ( =<<: ) (f : 'a -> 'b list) (x : 'a list) = List.bind x ~f

(* Functor map *)
let ( >>|: ) (x : 'a list) (f : 'a -> 'b) = List.map x ~f
let ( |<<: ) (f : 'a -> 'b) (x : 'a list) = List.map x ~f

(* Side-effect map *)
let ( <-<: ) (f : 'a -> unit) (x : 'a list) = List.iter ~f x
let ( >->: ) (x : 'a list) (f : 'a -> unit) = List.iter ~f x

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

(** Tuples **)

module Pair : sig
  type ('a, 'b) t = 'a * 'b

  val first : ('a, 'b) t -> 'a
  val second : ('a, 'b) t -> 'b
end = struct
  type ('a, 'b) t = 'a * 'b

  let first (x, _) = x
  let second (_, x) = x
end

module Triple : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  val first : ('a, 'b, 'c) t -> 'a
  val second : ('a, 'b, 'c) t -> 'b
  val third : ('a, 'b, 'c) t -> 'c
end = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let first (x, _, _) = x
  let second (_, x, _) = x
  let third (_, _, x) = x
end

(** Strings **)

module String = struct
  include String

  let non_stupid_slice (str : string) (start : int) (stop : int) =
    let len = length str in
    let m_start, m_stop = (start % len, stop % len) in
    if Int.equal m_start m_stop then make 0 (Char.unsafe_of_int 0)
    else slice str m_start m_stop
end

(** Other **)

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
