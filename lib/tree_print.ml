open Core

type tree_print_node =
  | Leaf of string
  | Branch of string option * tree_print_node list

let indent (indenter : string) (lines : string list) =
  List.map ~f:(( ^ ) indenter) lines

let rec tree_format_rec (indenter : string) (node : tree_print_node) =
  match node with
  | Leaf str -> [ str ]
  | Branch (None, branches) -> format_tree_branch indenter branches
  | Branch (Some label, branches) ->
      label :: format_tree_branch indenter branches

and format_tree_branch (indenter : string) (branches : tree_print_node list) =
  indent indenter @@ List.concat
  @@ List.map ~f:(tree_format_rec indenter) branches