open Core

type position = {
  row: int;
  col: int;
}
[@@deriving eq, sexp, show]

type location = {
  start: position;
  stop: position;
}
[@@deriving eq, sexp, show]

let loc_zero = {
  start = {
    row = 0;
    col = 0;
  };
  stop = {
    row = 0;
    col = 0;
  }
}

module Node = struct
  type 'node t = {
    pos: location;
    value: 'node;
  }
end

module Text = struct
  type word =
    | Word of string
    | Comma
    | Pipe
    | Whitespace
    | Linebreak

  type t = word list

end

module Math : sig
  type operator =
    | Plus
    | Minus
    | Times
    | Union
    | Inter
    | Frac
    | And
    | Or

  type unary =
    | Negate
    | Not

  type relation =
    | Le
    | Leq
    | Ge
    | Geq
    | Eq
    | In
    | NotIn
    | Subset
    | Superset
    | SubsetEq
    | SupersetEq
    | Implies
    | Iff

  type t =
    | Op of t * operator * t
    | Unary of unary * t
    | Rel of t * relation * t
    | Literal of int
    | Variable of string
    | Grouping of t
    | Apply of t * t list (* ex: f(args) (if args has length 1, could also be interpreted as multiplication) *)
    | Subscript of t * t (* not implemented as an binop since can be interpreted in different ways depending on context *)
    | Superscript of t * t
    | Command of string * t option
    | Forall of t * t (* forall X, Y *)
    | Exists of t * t (* exists X, Y *)
    | Suchthat of t (* s.t. X *)

  val pp: Format.formatter -> t -> unit

  val type_check: t -> unit

end = struct
  type operator =
    | Plus
    | Minus
    | Times
    | Union
    | Inter
    | Frac
    | And
    | Or

  type unary =
    | Negate
    | Not

  type relation =
    | Le
    | Leq
    | Ge
    | Geq
    | Eq
    | In
    | NotIn
    | Subset
    | Superset
    | SubsetEq
    | SupersetEq
    | Implies
    | Iff

  type t =
    | Op of t * operator * t
    | Unary of unary * t
    | Rel of t * relation * t
    | Literal of int
    | Variable of string
    | Grouping of t
    | Apply of t * t list
    | Subscript of t * t
    | Superscript of t * t
    | Command of string * t option
    | Forall of t * t
    | Exists of t * t
    | Suchthat of t

  let string_of_unary = function
    | Negate -> "-"
    | Not -> "NOT"

  let string_of_operator = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Frac -> "/"
    | Union -> "UNION"
    | Inter -> "INTER"
    | And -> "AND"
    | Or -> "OR"

  let string_of_relation = function
    | Le -> "<"
    | Leq -> "<="
    | Ge -> ">"
    | Geq -> ">="
    | Eq -> "="
    | In -> "IN"
    | NotIn -> "NOTIN"
    | Subset -> "SUBSET"
    | Superset -> "SUPERSET"
    | SubsetEq -> "SUBSETEQ"
    | SupersetEq -> "SUPERSETEQ"
    | Implies -> "IMPLIES"
    | Iff -> "IFF"

  let char_sep char formatter = fun () -> Format.pp_print_char formatter char

  let rec pp formatter math = match math with
      | Op (lhs, op, rhs) -> Format.fprintf formatter "(%s %a %a)" (string_of_operator op) pp lhs pp rhs
      | Unary (op, lhs) -> Format.fprintf formatter "(%s %a)" (string_of_unary op) pp lhs
      | Literal num -> Format.fprintf formatter "%i" num
      | Variable var -> Format.fprintf formatter "%s" var
      | Grouping expr -> Format.fprintf formatter "%a" pp expr
      | Rel (lhs, rel, rhs) -> Format.fprintf formatter "(%s %a %a)" (string_of_relation rel) pp lhs pp rhs
      | Apply (lhs, rhs) -> Format.fprintf formatter "(%a %a)"  pp lhs (Format.pp_print_list ~pp_sep:(char_sep ' ') pp) rhs
      | Subscript (lhs, rhs) -> Format.fprintf formatter "%a_%a"  pp lhs pp rhs
      | Superscript (lhs, rhs) -> Format.fprintf formatter "%a^%a"  pp lhs pp rhs
      | Command (name, arg) -> (match arg with
        | Some thing -> Format.fprintf formatter "%s{%a}" name pp thing
        | None -> Format.fprintf formatter "%s" name
      )
      | Forall (expr, next) -> Format.fprintf formatter "(FORALL %a, %a)" pp expr pp next
      | Exists (expr, next) -> Format.fprintf formatter "(EXISTS %a %a)" pp expr pp next
      | Suchthat expr -> Format.fprintf formatter "SUCHTHAT %a" pp expr

    let type_check math =
      let vars = ref (Map.empty (module String)) in (* map from variable names to types *)
      let fns = ref (Map.empty (module String)) in (* map from function names to type signatures *)
      let constraints = ref [] in

      let add_constraint (c: Typing.type_constraint) = constraints := c :: !constraints in
      let add_var name t = vars := Map.set !vars ~key:name ~data:t in
      let add_fns name t = fns := Map.set !fns ~key:name ~data:t in

      let rec recurse node =
        match node with
        | Op (lhs, op, rhs) -> (match op with
          | Plus | Minus | Times | Frac -> (
            let lhs_t = recurse lhs in
            let rhs_t = recurse rhs in
            add_constraint (lhs_t, Typing.Number);
            add_constraint (rhs_t, Typing.Number);
            Typing.Number
          )
          | Union | Inter -> (
            let t = Typing.fresh () in
            let lhs_t = recurse lhs in
            let rhs_t = recurse rhs in
            add_constraint (lhs_t, Typing.Set (Typing.Any t));
            add_constraint (rhs_t, Typing.Set (Typing.Any t));
            Typing.Set (Typing.Any t)
          )
          | And | Or -> (
            let lhs_t = recurse lhs in
            let rhs_t = recurse rhs in
            add_constraint (lhs_t, Typing.Bool);
            add_constraint (rhs_t, Typing.Bool);
            Typing.Bool
          )
        )
        | Unary (op, lhs) -> (match op with
          | Negate -> (
            let lhs_t = recurse lhs in
            add_constraint (lhs_t, Typing.Number);
            Typing.Number

          )
          | Not -> (
            let lhs_t = recurse lhs in
            add_constraint (lhs_t, Typing.Bool);
            Typing.Bool
          )
        )
        | Literal _ -> Typing.Number
        | Variable name -> (
          (* TODO: "special" variable names such as e, \pi, and \mathbb{R} *)
          match Map.find !vars name with
          | Some t -> Typing.Any t
          | None -> (
            let t = Typing.fresh () in
            add_var name t;
            Typing.Any t
          )
        )
        | Grouping expr -> (
          recurse expr
        )
        | Rel (lhs, rel, rhs) -> (
          (* TODO: ignore eq/ineq chaining for now *)
          match rel with
          | Eq -> (
            let lhs_t = recurse lhs in
            let rhs_t = recurse rhs in
            add_constraint (lhs_t, rhs_t);
            Typing.Bool
          )
          | Le | Leq | Ge | Geq -> (
            let lhs_t = recurse lhs in
            let rhs_t = recurse rhs in
            add_constraint (lhs_t, Typing.Number);
            add_constraint (rhs_t, Typing.Number);
            Typing.Bool
          )
          | In | NotIn -> (
            let lhs_t = recurse lhs in
            let rhs_t = recurse rhs in
            add_constraint (rhs_t, Typing.Set lhs_t);
            Typing.Bool
          )
          | Subset | SubsetEq | Superset | SupersetEq -> (
            let lhs_t = recurse lhs in
            let rhs_t = recurse rhs in
            add_constraint (rhs_t, Typing.Set lhs_t);

            (* ensure both lhs and rhs are sets *)
            let t0 = Typing.fresh () in
            let t1 = Typing.fresh () in
            add_constraint (lhs_t, Typing.Set (Typing.Any t0));
            add_constraint (rhs_t, Typing.Set (Typing.Any t1));

            Typing.Bool
          )
          | Implies | Iff -> (
            let lhs_t = recurse lhs in
            let rhs_t = recurse rhs in
            add_constraint (lhs_t, Typing.Bool);
            add_constraint (rhs_t, Typing.Bool);
            Typing.Bool
          )
        )
        | Apply (lhs, args) -> (
          let args_t, return_t = (match lhs with
            | Variable name -> (
              match Map.find !fns name with
              | Some data -> data
              | None -> (
                let args_t = List.map args ~f:(fun _ -> Typing.fresh ()) in
                let return_t = Typing.fresh () in
                add_fns name (args_t, return_t);
                (args_t, return_t)
              )
            )
            | _ -> raise (Typing.TypeError "Cannot apply non-function")
          ) in
          List.iter (List.zip_exn args args_t) ~f:(fun (x, y) -> add_constraint (recurse x, Typing.Any y));
          Typing.Any return_t
        )
        | _ -> raise (Typing.TypeError "Not yet implemented")
        (* | Subscript (lhs, rhs) -> Format.fprintf formatter "%a_%a"  pp lhs pp rhs *)
        (* | Superscript (lhs, rhs) -> Format.fprintf formatter "%a^%a"  pp lhs pp rhs *)
        (* | Command (name, arg) -> (match arg with *)
        (*   | Some thing -> Format.fprintf formatter "%s{%a}" name pp thing *)
        (*   | None -> Format.fprintf formatter "%s" name *)
        (* ) *)
        (* | Forall (expr, next) -> Format.fprintf formatter "(FORALL %a, %a)" pp expr pp next *)
        (* | Exists (expr, next) -> Format.fprintf formatter "(EXISTS %a %a)" pp expr pp next *)
        (* | Suchthat expr -> Format.fprintf formatter "SUCHTHAT %a" pp expr *)
      in
      let _ = recurse math in

      let subs = Typing.unify !constraints in

      Map.iteri !vars ~f:(fun ~key ~data ->
        let principal_type = Typing.apply subs (Typing.Any data) in
        Format.printf "val %s : %a\n" key Typing.pp_math_type principal_type;
      );

      Map.iteri !fns ~f:(fun ~key ~data ->
        let (args_t, ret_t) = data in
        let arg_types = List.map args_t ~f:(fun a -> Typing.apply subs (Typing.Any a)) in
        let return_type  = Typing.apply subs (Typing.Any ret_t) in
        Format.printf "fun %s : %a -> %a\n" key (Format.pp_print_list ~pp_sep:(fun f -> fun () -> Format.pp_print_string f " -> ") Typing.pp_math_type) arg_types Typing.pp_math_type return_type
      );

      ()
end

module rec Environment : sig
  (* store both begin and end name to verify environment is valid *)
  type t = (string * string) * Latex.t list

  val name: Environment.t -> string
  val name_end: Environment.t -> string
  val body: Environment.t -> Latex.t list
end = struct
  type t = (string * string) * Latex.t list

  let name env = fst (fst env)
  let name_end env = snd (fst env)
  let body = snd
end
and Mathmode: sig
  type t = string
end = struct
  type t = string
end
and Latex: sig
  type latex =
    | Text of Text.t
    | Environment of Environment.t
    | Mathmode of Mathmode.t

  type t = latex Node.t

  val pp: Format.formatter -> t -> unit
end = struct
  type latex =
    | Text of Text.t
    | Environment of Environment.t
    | Mathmode of Mathmode.t

  type t = latex Node.t

  module PrettyPrinter = struct
    let pp_text formatter (text: Text.t) =
      let string_of_word = function
        | Text.Word x -> sprintf "WORD[%s]" x
        | Text.Comma -> "COMMA"
        | Text.Pipe -> "PIPE"
        | Text.Whitespace -> " "
        | Text.Linebreak -> "\n"
      in
      let strings = List.map ~f:string_of_word text in
      Format.fprintf formatter "TEXT[%s]" (String.concat ~sep:"" strings)

    let rec pp_environment formatter (env: Environment.t) =
      let contents = Format.pp_print_list pp_latex in
      Format.fprintf formatter "%s[\n%a\n]" (Environment.name env) contents (snd env)
    and pp_mathmode formatter math = Format.fprintf formatter "MATH[%s]" math
    and pp_latex formatter (latex: t) = match latex with
    | {pos = _; value = Text text} -> pp_text formatter text
    | {pos = _; value = Environment env} -> pp_environment formatter env
    | {pos = _; value = Mathmode math} -> pp_mathmode formatter math
end

  let pp formatter latex = Format.fprintf formatter "%a" PrettyPrinter.pp_latex latex
end

