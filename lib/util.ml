open Core
open Fn 
open Re.Str

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


(** [str_split str separator] splits the provided string [str] on a provided 
    separator [sep] and returns a list of split strings
    @param str string to split
    @param sep separator string 
    @return splitted list of string on separator
    *)    
let str_split (str: string) (sep: string) : string list =
  split (regexp_string sep) str


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
  

let extract_patterns (filename: string) = 
    let content = read_file_as_str filename in
      let splits = str_split content "\"," in
        let def = List.nth splits 0 in 
          match def with 
          | Some _ -> [1; 2; 3] 
          | None -> []
      (* List.iter splits ~f:(fun split -> print_endline ("here: " ^ split)) *)
