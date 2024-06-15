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
let ( <-<? ) (f : 'a -> unit) (x : 'a option) = Option.iter ~f: f x

(** List monad **)

(* Monadic bind *)
let ( >>=: ) (x : 'a list) (f : 'a -> 'b list) = List.bind x ~f
let ( =<<: ) (f : 'a -> 'b list) (x : 'a list) = List.bind x ~f

(* Functor map *)
let ( >>|: ) (x : 'a list) (f : 'a -> 'b) = List.map x ~f
let ( |<<: ) (f : 'a -> 'b) (x : 'a list) = List.map x ~f

(* Side-effect map *)
let ( <-<: ) (f : 'a -> unit) (x : 'a list) = List.iter ~f: f x

let pp_hashtbl formatter ~pp_key ~pp_data t =
  let pp_pair formatter (k, d) =
    Format.fprintf formatter "(%a -> %a)" pp_key k pp_data d
  in
  Format.fprintf formatter "[%a]"
    (Format.pp_print_list ~pp_sep:(string_sep ", ") pp_pair)
    (Hashtbl.to_alist t)



(* let extract_patterns (filename: string) = 
  let read_file (filename: string) =
    In_channel.with_file filename ~f:(fun input_c ->
      In_channel.iter_lines input_c ~f:(fun line -> 
       print_endline line
      )
    )
    in read_file filename  *)

let read_file_as_str (filename: string) = 
   let buffer = Buffer.create 4096 in 
    In_channel.with_file filename ~f:(fun input_c ->
      In_channel.iter_lines input_c ~f:(fun line -> 
          Buffer.add_string buffer line;
          Buffer.add_char buffer '\n'
        )
  );
  Buffer.contents buffer

let extract_patterns (filename: string) = 
    let content = read_file_as_str filename in
    print_endline content
