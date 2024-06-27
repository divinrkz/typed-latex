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
