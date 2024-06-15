open Core
open Ast
open Util

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

let relation_name (relation : Math.relation) =
  match relation with
  | Le -> "Le"
  | Leq -> "Leq"
  | Ge -> "Ge"
  | Geq -> "Geq"
  | Eq -> "Eq"
  | In -> "In"
  | NotIn -> "NotIn"
  | Subset -> "Subset"
  | Superset -> "Superset"
  | SubsetEq -> "SubsetEq"
  | SupersetEq -> "SupersetEq"
  | Equiv _ -> "Equiv"

let tree_format (indenter : string) (node : tree_print_node) =
  List.fold ~f:(fun x y -> x ^ "\n" ^ y) ~init:""
  @@ tree_format_rec indenter node

let rec latex_to_string_tree (node : Latex.t) =
  match User.unwrap_node node with
  | Latex.Command (name, children) ->
      Branch (Some ("Command: " ^ name), latex_to_string_tree |<<: children)
  | Latex.Environment (name, args, children) ->
      Branch
        ( Some ("Environment: " ^ name),
          [
            Branch (Some "@args", latex_to_string_tree |<<: args);
            Branch (Some "@children", latex_to_string_tree |<<: children);
          ] )
  | Latex.Latex children ->
      Branch (Some "Latex", latex_to_string_tree |<<: children)
  | Latex.Mathmode _ as math_latex ->
      Branch
        (Some "Math", latex_math_to_string_tree |<<: User.parse_math math_latex)
  | Latex.Multiline _ as math_latex ->
      Branch
        ( Some "Multiline",
          latex_math_to_string_tree |<<: User.parse_math math_latex )
  | Latex.Newline -> Leaf "Newline"
  | Latex.Whitespace -> Leaf "Whitespace"
  | Latex.Word word -> Leaf ("Word: " ^ word)

and latex_math_to_string_tree (math : Math.t) =
  match math with
  | Op (left, op, right) ->
      Branch
        ( Some ("Op: " ^ Math.string_of_operator op),
          [ latex_math_to_string_tree left; latex_math_to_string_tree right ] )
  | Unary (op, arg) ->
      Branch
        ( Some ("Unary: " ^ Math.string_of_unary op),
          [ latex_math_to_string_tree arg ] )
  | Relation (bound, relations) ->
      Branch
        ( Some "Relation",
          Branch (Some "@bound", [ latex_math_to_string_tree bound ])
          :: (latex_math_relation_to_string_tree |<<: relations) )
  | LogicOp (left, op, right) ->
      Branch
        ( Some ("LogicOp: " ^ Math.string_of_logic_op op),
          [ latex_math_to_string_tree left; latex_math_to_string_tree right ] )
  | Logic (bound, relations) ->
      Branch
        ( Some "Logic",
          Branch (Some "@bound", [ latex_math_to_string_tree bound ])
          :: (latex_math_logic_to_string_tree |<<: relations) )
  | IntLiteral num -> Leaf (string_of_int num)
  | FloatLiteral num -> Leaf (string_of_float num)
  | SetComprehension (expr, relation) ->
      Branch
        ( Some "SetComprehension",
          [
            Branch (Some "@expr", [ latex_math_to_string_tree expr ]);
            Branch (Some "@relation", [ latex_math_to_string_tree relation ]);
          ] )
  | SetLiteral children ->
      Branch (Some "SetLiteral", latex_math_to_string_tree |<<: children)
  | Variable var -> Leaf ("Variable: " ^ var)
  | Grouping child ->
      Branch (Some "Grouping", [ latex_math_to_string_tree child ])
  | Apply (f, args) ->
      Branch
        ( Some "Apply",
          [
            Branch (Some "@function", [ latex_math_to_string_tree f ]);
            Branch (Some "@args", latex_math_to_string_tree |<<: args);
          ] )
  | Subscript (main, sub) ->
      Branch
        ( Some "Subscript",
          [
            Branch (Some "@main", [ latex_math_to_string_tree main ]);
            Branch (Some "@sub", [ latex_math_to_string_tree sub ]);
          ] )
  | Superscript (main, sup) ->
      Branch
        ( Some "Subscript",
          [
            Branch (Some "@main", [ latex_math_to_string_tree main ]);
            Branch (Some "@sup", [ latex_math_to_string_tree sup ]);
          ] )
  | Command (name, arg) ->
      Branch
        ( Some ("Command: " ^ name),
          Option.to_list (latex_math_to_string_tree |<<? arg) )
  | Forall (relation, dependent) ->
      Branch
        ( Some "Forall",
          [
            Branch (Some "@relation", [ latex_math_to_string_tree relation ]);
            Branch (Some "@dependent", [ latex_math_to_string_tree dependent ]);
          ] )
  | Exists (relation, dependent) ->
      Branch
        ( Some "Exists",
          [
            Branch (Some "@relation", [ latex_math_to_string_tree relation ]);
            Branch (Some "@dependent", [ latex_math_to_string_tree dependent ]);
          ] )
  | Suchthat relation ->
      Branch (Some "Suchthat", [ latex_math_to_string_tree relation ])
  | Text text -> Leaf ("Text: " ^ text)
  | Summation (lower, upper, addend) ->
      Branch
        ( Some "Summation",
          [
            Branch (Some "@lower", [ latex_math_to_string_tree lower ]);
            Branch (Some "@upper", [ latex_math_to_string_tree upper ]);
            Branch (Some "@addend", [ latex_math_to_string_tree addend ]);
          ] )
  | Tuple children ->
      Branch (Some "Tuple", latex_math_to_string_tree |<<: children)

and latex_math_relation_to_string_tree
    ((relation, child) : Math.relation * Math.t) =
  match relation with
  | Equiv equivalent ->
      Branch
        ( Some "@relation: Equiv",
          [
            Branch (Some "@equivalent", [ latex_math_to_string_tree equivalent ]);
            Branch (Some "@child", [ latex_math_to_string_tree child ]);
          ] )
  | _ ->
      Branch
        ( Some ("@relation: " ^ relation_name relation),
          [ latex_math_to_string_tree child ] )

and latex_math_logic_to_string_tree ((logic, child) : Math.logic * Math.t) =
  Branch
    ( Some ("@logic_op: " ^ Math.string_of_logic logic),
      [ latex_math_to_string_tree child ] )

let latex_tree_format = tree_format "| " << latex_to_string_tree
