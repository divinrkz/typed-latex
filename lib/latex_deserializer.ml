open Core
open Util
open Fn
module Json = Yojson.Basic

let type_key = "Type"

type json_map = Json.t String.Map.t

type json_type =
  | JsonAssoc
  | JsonBool
  | JsonFloat
  | JsonInt
  | JsonList
  | JsonNull
  | JsonString
[@@deriving eq, show, sexp, hash, ord]

module RawMathLatex = struct
  type t = Todo
end

module RawLatex : sig
  type t =
    | Latex of t list
    | Environment of string * t list
    | Math of RawMathLatex.t
    | MultilineMath of RawMathLatex.t
    | Macro of t list * t list
    | Text of string
    | Newline

  type parse_error =
    (* Program incomplete *)
    | NotImplemented
    (* Actuall errors *)
    | InvalidLatexNodeType of Json.t (* node (expected: assoc) *)
    | DuplicateAssocKey of (string * Json.t) list (* key name, assoc *)
    | NoTypeKey of json_map (* assoc *)
    | InvalidNodeTypeType of Json.t (* node type (expected: `String str) *)
    | UnknownNodeType of string (* node type *)
    | NoKey of json_map * string (* assoc, expected key *)
    | IncorrectAttrType of Json.t * json_type (* attr, expected type *)

  type t_res = (t, parse_error) result
end = struct
  type t =
    | Latex of t list
    | Environment of string * t list
    | Math of RawMathLatex.t
    | MultilineMath of RawMathLatex.t
    | Macro of t list * t list
    | Text of string
    | Newline

  type parse_error =
    (* Program incomplete *)
    | NotImplemented
    (* Actuall errors *)
    | InvalidLatexNodeType of Json.t (* node (expected: assoc) *)
    | DuplicateAssocKey of (string * Json.t) list (* key name, assoc *)
    | NoTypeKey of json_map (* assoc *)
    | InvalidNodeTypeType of Json.t (* node type (expected: `String str) *)
    | UnknownNodeType of string (* node type *)
    | NoKey of json_map * string (* assoc, expected key *)
    | IncorrectAttrType of Json.t * json_type (* attr, expected type *)

  type t_res = (t, parse_error) result
end

let json_assoc_to_map (json : Json.t) : (json_map, RawLatex.parse_error) result
    =
  match json with
  | `Assoc list_mapping ->
      RawLatex.DuplicateAssocKey list_mapping
      <!<! String.Map.of_alist_or_error list_mapping
  | _ -> Error (InvalidLatexNodeType json)

let json_map_get (mapping : json_map) (err : RawLatex.parse_error)
    (key : string) : (Json.t, RawLatex.parse_error) result =
  err <!<! Map.find_or_error mapping key

let json_map_get_attr (mapping : json_map) (key : string) :
    (Json.t, RawLatex.parse_error) result =
  json_map_get mapping (NoKey (mapping, key)) key

let rec deserialize_from_json (json : Json.t) : RawLatex.t_res =
  let mapping = json_assoc_to_map json in
  let type_val =
    json_map_get |<<! mapping <<|! RawLatex.InvalidLatexNodeType json
    <<=! "type"
  in
  (deserialize_assoc_type_val |<<! mapping) *=<<! type_val

and deserialize_assoc_type_val (mapping : json_map) (type_val : Json.t) :
    RawLatex.t_res =
  match type_val with
  | `String type_name -> deserialize_assoc_type mapping type_name
  | _ -> Error (RawLatex.InvalidNodeTypeType type_val)

and deserialize_assoc_type (mapping : json_map) (type_name : string) :
    RawLatex.t_res =
  match type_name with
  | "Latex" -> Error NotImplemented
  | "Environment" -> Error NotImplemented
  | "Math" -> Error NotImplemented
  | "MultilineMath" -> Error NotImplemented
  | "Macro" -> Error NotImplemented
  | "Text" -> deserialize_text =<<! json_map_get_attr mapping "Value"
  | "Newline" -> Ok Newline
  | _ -> Error (UnknownNodeType type_name)

(* and deserialize_latex (json : Json.t) : RawLatex.t_res =
    match json with
    | `String text -> Ok (Text text)
    | _ -> Error (IncorrectAttrType (json, JsonString)) *)
  
and deserialize_text (json : Json.t) : RawLatex.t_res =
  match json with
  | `String text -> Ok (Text text)
  | _ -> Error (IncorrectAttrType (json, JsonString))
