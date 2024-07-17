open Core
open Latex_deserializer

let rec unwrap_to_document (node : RawLatex.t) =
  match node with
  | RawLatex.Environment ("document", _) -> Some node
  | RawLatex.Latex children -> List.find_map ~f:unwrap_to_document children
  | _ -> None