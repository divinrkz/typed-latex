open Core
open Fn

let string_sep str formatter = fun () -> Format.pp_print_string formatter str

let (<<) = compose

(** Option monad **)
(* Monadic bind *)
let (>>=?) (x: 'a option) (f: 'a -> 'b option) = Option.bind x ~f
let (=<<?) (f: 'a -> 'b option) (x: 'a option) = Option.bind x ~f
(* Functor map *)
let (>>|?) (x: 'a option) (f: 'a -> 'b) = Option.map x ~f
let (|<<?) (f: 'a -> 'b) (x: 'a option) = Option.map x ~f

(** List monad **)
(* Monadic bind *)
let (>>=:) (x: 'a list) (f: 'a -> 'b list) = List.bind x ~f
let (=<<:) (f: 'a -> 'b list) (x: 'a list) = List.bind x ~f
(* Functor map *)
let (>>|:) (x: 'a list) (f: 'a -> 'b) = List.map x ~f
let (|<<:) (f: 'a -> 'b) (x: 'a list) = List.map x ~f

let pp_hashtbl formatter ~pp_key ~pp_data t =
  let pp_pair formatter (k, d) = Format.fprintf formatter "(%a -> %a)" pp_key k pp_data d in
  Format.fprintf formatter "[%a]" (Format.pp_print_list ~pp_sep:(string_sep ", ") pp_pair) (Hashtbl.to_alist t);
