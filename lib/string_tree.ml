open Core

type string_tree = Leaf of string | Branch of string option * string_tree list

let indent (indenter : string) (lines : string list) =
  List.map ~f:(( ^ ) indenter) lines

let rec tree_format_rec (indenter : string) (node : string_tree) =
  match node with
  | Leaf str -> [ str ]
  | Branch (None, branches) -> format_tree_branch indenter branches
  | Branch (Some label, branches) ->
      label :: format_tree_branch indenter branches

and format_tree_branch (indenter : string) (branches : string_tree list) =
  indent indenter @@ List.concat
  @@ List.map ~f:(tree_format_rec indenter) branches

let tree_format (indenter : string) (node : string_tree) = 
  List.fold ~f:(fun x y -> x ^ "\n" ^ y) ~init:""
  @@ tree_format_rec indenter node
