open Core
open Util
open String_tree
module Json = Yojson.Basic

let type_key = "type"

type json_map = Json.t String.Map.t

type json_type =
  | JsonAssoc
  | JsonBool
  | JsonFloat
  | JsonInt
  | JsonList
  | JsonNull
  | JsonString
  | Comment

let string_of_json_type (json_t : json_type) =
  match json_t with
  | JsonAssoc -> "Assoc"
  | JsonBool -> "Bool"
  | JsonFloat -> "Float"
  | JsonInt -> "Int"
  | JsonList -> "List"
  | JsonNull -> "Null"
  | JsonString -> "String"
  | Comment -> "Comment"

module RawMathLatex : sig
  type t = string_tree
  type parse_error
  type t_res = (t, parse_error) result

  val string_of_parse_error : parse_error -> string
  val parse : string -> t_res
end = struct
  type t = string_tree

  type lex_token =
    | StrToken of string
    | LeftParToken
    | RightParToken
    | CommaToken
  [@@deriving eq, show, sexp, hash, ord]

  type parse_error =
    | UnexpectedSymbol of lex_token * lex_token list
    | UnbalancedParens
    | EmptyExpression

  let string_of_parse_error (err : parse_error) =
    match err with
    | UnexpectedSymbol (token, _) -> "Unexpected token: " ^ show_lex_token token
    | UnbalancedParens -> "Unbalanced parens"
    | EmptyExpression -> "Empty math expression"

  type t_res = (t, parse_error) result
  type context = t * lex_token list
  type context_res = (context, parse_error) result

  let rec lex_rec (str : string) (start : int) (stop : int) =
    match String.opt_get str stop with
    | Some ',' ->
        StrToken (String.non_stupid_slice str start stop)
        :: CommaToken
        :: lex_rec str (stop + 1) (stop + 1)
    | Some '(' ->
        StrToken (String.non_stupid_slice str start stop)
        :: LeftParToken
        :: lex_rec str (stop + 1) (stop + 1)
    | Some ')' ->
        StrToken (String.non_stupid_slice str start stop)
        :: RightParToken
        :: lex_rec str (stop + 1) (stop + 1)
    | Some _ -> lex_rec str start (stop + 1)
    | None -> []

  let clean_token (token : lex_token) : lex_token list =
    match token with
    | StrToken symbol ->
        let clean_string =
          String.strip
            ~drop:(List.mem ~equal:Char.equal [ ' '; '\n'; '\t' ])
            symbol
        in
        if String.is_empty clean_string then [] else [ StrToken clean_string ]
    | _ -> [ token ]

  let lex (str : string) : lex_token list = clean_token =<<: lex_rec str 0 0

  let consume_token (token : lex_token)
      (on_other : (lex_token list, parse_error) result option)
      (on_empty : (lex_token list, parse_error) result option)
      ((out, tokens) : 'a * lex_token list) :
      ('a * lex_token list, parse_error) result =
    match tokens with
    | [] -> Pair.build out |<<! Option.value ~default:(Ok []) on_empty
    | head :: tail when equal_lex_token head token -> Ok (out, tail)
    | _ -> Pair.build out |<<! Option.value ~default:(Ok tokens) on_other

  let rec parse_rec (tokens : lex_token list) : context_res =
    match tokens with
    | StrToken f_name :: LeftParToken :: tail ->
        let build_branch ((children, new_tail) : t list * lex_token list) =
          (Branch (Some f_name, children), new_tail)
        in
        build_branch |<<! parse_args_rec tail
    | StrToken symbol :: tail -> Ok (Leaf symbol, tail)
    | symbol :: _ -> Error (UnexpectedSymbol (symbol, tokens))
    | [] -> Error EmptyExpression

  and parse_args_rec (tokens : lex_token list) :
      (t list * lex_token list, parse_error) result =
    match tokens with
    | RightParToken :: tail -> Ok ([], tail)
    | LeftParToken :: _ -> Error (UnexpectedSymbol (LeftParToken, tokens))
    | CommaToken :: _ -> Error (UnexpectedSymbol (CommaToken, tokens))
    | StrToken _ :: _ ->
        let head_parse = parse_rec tokens in
        let head_comma_parse =
          consume_token CommaToken None (Some (Error UnbalancedParens))
          =<<! head_parse
        in
        let tail_parse =
          parse_args_rec =<<! (Pair.second |<<! head_comma_parse)
        in
        let arg_parse =
          (List.cons |<<! (Pair.first |<<! head_comma_parse))
          *<<! (Pair.first |<<! tail_parse)
        in
        (Pair.build |<<! arg_parse) *<<! (Pair.second |<<! tail_parse)
    | [] -> Error UnbalancedParens

  let assert_empty_tokens ((out, tokens) : context) : t_res =
    match tokens with
    | [] -> Ok out
    | hd :: _ -> Error (UnexpectedSymbol (hd, tokens))

  let parse (math_str : string) : t_res =
    let lexed = lex math_str in
    let parsed = parse_rec lexed in
    assert_empty_tokens =<<! parsed
end

module RawLatex : sig
  type t =
    | Latex of t list
    | Environment of string * t list
    | Math of RawMathLatex.t
    | MultilineMath of RawMathLatex.t list
    | Macro of string * t list
    | Text of string
    | Comment of string

  type parse_error =
    (* Program incomplete *)
    | NotImplemented
    (* Actuall errors *)
    | InvalidLatexNodeType of Json.t (* node (expected: assoc) *)
    | DuplicateAssocKey of Json.t (* node *)
    | InvalidNodeTypeType of Json.t (* node type (expected: `String str) *)
    | UnknownNodeType of string (* node type *)
    | NoKey of json_map * string (* assoc, expected key *)
    | IncorrectAttrType of Json.t * json_type (* attr, expected type *)
    | MathParseError of
        string * RawMathLatex.parse_error (* math string, error *)

  val string_of_parse_error : parse_error -> string

  type t_res = (t, parse_error) result

  val tree_format : t -> string
  val deserialize_from_json : Json.t -> t_res
end = struct
  type t =
    | Latex of t list
    | Environment of string * t list
    | Math of RawMathLatex.t
    | MultilineMath of RawMathLatex.t list
    | Macro of string * t list
    | Text of string
    | Comment of string

  let escape_string (str : string) : string = String.escaped str

  let rec to_string_tree (latex : t) =
    match latex with
    | Latex children -> Branch (Some "Latex", to_string_tree |<<: children)
    | Environment (name, children) ->
        Branch (Some ("Environment: " ^ name), to_string_tree |<<: children)
    | Macro (name, args) ->
        Branch (Some ("Macro: " ^ name), to_string_tree |<<: args)
    | Text text -> Leaf ("Text: \"" ^ escape_string text ^ "\"")
    | Comment comment -> Leaf ("Comment: " ^ comment)
    | Math math_tree -> Branch (Some "Math", [ math_tree ])
    | MultilineMath math_trees -> Branch (Some "Multiline Math", math_trees)

  let tree_format = tree_format "| " << to_string_tree

  type parse_error =
    (* Program incomplete *)
    | NotImplemented
    (* Actuall errors *)
    | InvalidLatexNodeType of Json.t (* node (expected: assoc) *)
    | DuplicateAssocKey of Json.t (* node *)
    | InvalidNodeTypeType of Json.t (* node type (expected: `String str) *)
    | UnknownNodeType of string (* node type *)
    | NoKey of json_map * string (* assoc, expected key *)
    | IncorrectAttrType of Json.t * json_type (* attr, expected type *)
    | MathParseError of
        string * RawMathLatex.parse_error (* math string, error *)

  let json_debug_to_string : Json.t -> string = Json.pretty_to_string

  let string_of_parse_error (err : parse_error) =
    match err with
    | NotImplemented -> "Not yet implemented"
    | InvalidLatexNodeType node ->
        "A latex ast node is expected to be a json assoc. Instead, it is:\n"
        ^ json_debug_to_string node
    | DuplicateAssocKey node ->
        "Duplicate key in json assoc:\n" ^ json_debug_to_string node
    | InvalidNodeTypeType node ->
        "Expected the \"type\" attribute of a node json to be a string in:\n"
        ^ json_debug_to_string node
    | UnknownNodeType node_type ->
        "Unknown latex node type \"" ^ node_type ^ "\"."
    | NoKey (assoc, key) ->
        "Expected an attribute named \"" ^ key ^ "\" in:\n"
        ^ json_debug_to_string (`Assoc (Map.to_alist assoc))
    | IncorrectAttrType (node, expected_type) ->
        "Expected a json "
        ^ string_of_json_type expected_type
        ^ ", not:\n" ^ json_debug_to_string node
    | MathParseError (math_str, err) ->
        "Error in parsing $" ^ math_str ^ "$: "
        ^ RawMathLatex.string_of_parse_error err

  type t_res = (t, parse_error) result

  let json_assoc_to_map (json : Json.t) : (json_map, parse_error) result =
    match json with
    | `Assoc list_mapping ->
        DuplicateAssocKey json <!!<!! String.Map.of_alist_or_error list_mapping
    | _ -> Error (InvalidLatexNodeType json)

  let json_map_get (mapping : json_map) (err : parse_error) (key : string) :
      (Json.t, parse_error) result =
    err <!!<!! Map.find_or_error mapping key

  let json_map_get_attr (mapping : json_map) (key : string) :
      (Json.t, parse_error) result =
    json_map_get mapping (NoKey (mapping, key)) key

  let rec deserialize_from_json (json : Json.t) : t_res =
    let mapping = json_assoc_to_map json in
    let type_val = json_map_get_attr |<<! mapping <<=! type_key in
    (deserialize_assoc_type_val |<<! mapping) *=<<! type_val

  and deserialize_assoc_type_val (mapping : json_map) (type_val : Json.t) :
      t_res =
    match type_val with
    | `String type_name -> deserialize_assoc_type mapping type_name
    | _ -> Error (InvalidNodeTypeType type_val)

  and deserialize_assoc_type (mapping : json_map) (type_name : string) : t_res =
    match type_name with
    | "Latex" -> deserialize_latex =<<! json_map_get_attr mapping "children"
    | "Environment" ->
        (deserialize_environment |<<! json_map_get_attr mapping "name")
        *=<<! json_map_get_attr mapping "children"
    | "Math" -> deserialize_math =<<! json_map_get_attr mapping "value"
    | "MultilineMath" ->
        deserialize_multiline_math =<<! json_map_get_attr mapping "value"
    | "Macro" ->
        (deserialize_macro |<<! json_map_get_attr mapping "name")
        *=<<! json_map_get_attr mapping "args"
    | "Text" -> deserialize_text =<<! json_map_get_attr mapping "value"
    | "Comment" -> deserialize_comment =<<! json_map_get_attr mapping "value"
    | _ -> Error (UnknownNodeType type_name)

  and deserialize_latex (children_json : Json.t) : t_res =
    match children_json with
    | `List child_nodes ->
        let children_des = deserialize_from_json |<<: child_nodes in
        let children_overall = Result.all children_des in
        let latex_gen children = Latex children in
        latex_gen |<<! children_overall
    | _ -> Error (IncorrectAttrType (children_json, JsonList))

  and deserialize_environment (name_json : Json.t) (children_json : Json.t) :
      t_res =
    match (name_json, children_json) with
    | `String name, `List child_nodes ->
        let children_des = deserialize_from_json |<<: child_nodes in
        let children_overall = Result.all children_des in
        let environment_gen children = Environment (name, children) in
        environment_gen |<<! children_overall
    | `String _, _ -> Error (IncorrectAttrType (name_json, JsonString))
    | _ -> Error (IncorrectAttrType (children_json, JsonList))

  and deserialize_macro (name_json : Json.t) (args_json : Json.t) : t_res =
    match (name_json, args_json) with
    | `String name, `List arg_nodes ->
        let args_des = deserialize_from_json |<<: arg_nodes in
        let args_overall = Result.all args_des in
        let environment_gen args = Macro (name, args) in
        environment_gen |<<! args_overall
    | `String _, _ -> Error (IncorrectAttrType (args_json, JsonList))
    | _ -> Error (IncorrectAttrType (name_json, JsonString))

  and deserialize_text (text_json : Json.t) : t_res =
    match text_json with
    | `String text -> Ok (Text text)
    | _ -> Error (IncorrectAttrType (text_json, JsonString))

  and deserialize_comment (comment_json : Json.t) : t_res =
    match comment_json with
    | `String text -> Ok (Comment text)
    | _ -> Error (IncorrectAttrType (comment_json, JsonString))

  and raw_deserialize_math (math_json : Json.t) :
      (RawMathLatex.t, parse_error) result =
    match math_json with
    | `String math_str ->
        let math_parse_err_gen (err : RawMathLatex.parse_error) =
          MathParseError (math_str, err)
        in
        math_parse_err_gen |<<!! RawMathLatex.parse math_str
    | _ -> Error (IncorrectAttrType (math_json, JsonString))

  and deserialize_math (math_json : Json.t) : t_res =
    let math_gen (math : RawMathLatex.t) = Math math in
    math_gen |<<! raw_deserialize_math math_json

  and deserialize_multiline_math (math_children_json : Json.t) : t_res =
    match math_children_json with
    | `List math_strs ->
        let math_children = raw_deserialize_math |<<: math_strs in
        let math_children_overall = Result.all math_children in
        let multiline_math_gen (maths : RawMathLatex.t list) =
          MultilineMath maths
        in
        multiline_math_gen |<<! math_children_overall
    | _ -> Error (IncorrectAttrType (math_children_json, JsonList))
end
