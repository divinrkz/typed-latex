module Json = Yojson.Basic

module RawLatex = struct
  type t =
  | Str of string

  type parse_error =
  | UnknownError
  | NotImplemented of Json.t

  type t_res = (t, parse_error) result
end

let rec deserialize_from_json (json : Json.t): RawLatex.t_res =
  match json with
  | `Null -> Error UnknownError
  | `Assoc list_mapping -> 
  | _ -> Error (NotImplemented json)
let rec deserialize_assoc ()