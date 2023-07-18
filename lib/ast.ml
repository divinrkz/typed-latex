open Core
open Util

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

  type unary =
    | Negate
    | Not
    | Abs

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
    | And
    | Or

  type t =
    | Op of t * operator * t
    | Unary of unary * t
    (* Note: Relation does not keep track of precedence w.r.t and/or/implies/iff *)
    | Relation of t * (relation * t) list (* LHS (= RHS)+ *)
    | Literal of int
    | SetComprehension of t * t (* { expr | relation }*)
    | SetLiteral of t list (* { expr, expr, ... } *)
    | Variable of string
    | Grouping of t
    | Apply of t * t list (* ex: f(args) (if args has length 1, could also be interpreted as multiplication) *)
    | Subscript of t * t (* not implemented as an binop since can be interpreted in different ways depending on context *)
    | Superscript of t * t
    (* TODO: encode all possible commands in an enum *)
    | Command of string * t option
    | Forall of t * t (* forall X, Y *)
    | Exists of t * t (* exists X, Y *)
    | Suchthat of t (* s.t. X *)
    | Text of string

  val pp: Format.formatter -> t -> unit

  val type_check: t list -> unit

end = struct
  type operator =
    | Plus
    | Minus
    | Times
    | Union
    | Inter
    | Frac

  type unary =
    | Negate
    | Not
    | Abs

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
    | And
    | Or

    (* a iff b and c iff d *)
  type t =
    | Op of t * operator * t
    | Unary of unary * t
    | Relation of t * (relation * t) list
    | Literal of int
    | SetComprehension of t * t (* { expr | relation }*)
    | SetLiteral of t list (* { expr, expr, ... } *)
    | Variable of string
    | Grouping of t
    | Apply of t * t list
    | Subscript of t * t
    | Superscript of t * t
    | Command of string * t option
    | Forall of t * t
    | Exists of t * t
    | Suchthat of t
    | Text of string

  let string_of_unary = function
    | Negate -> "-"
    | Not -> "NOT"
    | Abs -> "ABS"

  let string_of_operator = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Frac -> "/"
    | Union -> "UNION"
    | Inter -> "INTER"

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
    | And -> "AND"
    | Or -> "OR"

  type state = {
    mutable le: bool;
    mutable ge: bool;
    mutable sub: bool;
    mutable sup: bool;
  }

  let is_greek_letter name =
    match name with
    | "\\alpha" | "\\nu" 
    | "\\beta" | "\\xi" | "\\Xi"
    | "\\gamma" | "\\Gamma"
    | "\\delta" | "\\Delta"	| "\\pi" | "\\Pi"
    | "\\epsilon" | "\\varepsilon" | "\\rho" | "\\varrho" 
    | "\\zeta" | "\\sigma" | "\\Sigma"
    | "\\eta" | "\\tau" 
    | "\\theta" | "\\vartheta" | "\\Theta" | "\\upsilon" | "\\Upsilon"
    | "\\iota" | "\\phi" | "\\varphi" | "\\Phi"
    | "\\kappa" | "\\chi" 
    | "\\lambda" | "\\Lambda"	| "\\psi" | "\\Psi"
    | "\\mu" | "\\omega" | "\\Omega" -> true
    | _ -> false

  let rec pp formatter math = match math with
      | Op (lhs, op, rhs) -> Format.fprintf formatter "(%s %a %a)" (string_of_operator op) pp lhs pp rhs
      | Unary (op, lhs) -> Format.fprintf formatter "(%s %a)" (string_of_unary op) pp lhs
      | Literal num -> Format.fprintf formatter "%i" num
      | Variable var -> Format.fprintf formatter "%s" var
      | Grouping expr -> Format.fprintf formatter "%a" pp expr
      | Relation (lhs, rhs) -> (
        let pp_pair = fun formatter -> fun (rel, t) -> Format.fprintf formatter "%s %a" (string_of_relation rel) pp t in
        Format.fprintf formatter "(%a %a)" pp lhs (Format.pp_print_list ~pp_sep:(string_sep " ") pp_pair) rhs;
      )
      | Apply (lhs, rhs) -> Format.fprintf formatter "(%a %a)"  pp lhs (Format.pp_print_list ~pp_sep:(string_sep " ") pp) rhs
      | Subscript (lhs, rhs) -> Format.fprintf formatter "%a_%a"  pp lhs pp rhs
      | Superscript (lhs, rhs) -> Format.fprintf formatter "%a^%a"  pp lhs pp rhs
      | Command (name, arg) -> (match arg with
        | Some thing -> Format.fprintf formatter "%s{%a}" name pp thing
        | None -> Format.fprintf formatter "%s" name
      )
      | Forall (expr, next) -> Format.fprintf formatter "(FORALL %a, %a)" pp expr pp next
      | Exists (expr, next) -> Format.fprintf formatter "(EXISTS %a %a)" pp expr pp next
      | Suchthat expr -> Format.fprintf formatter "SUCHTHAT %a" pp expr
      | SetLiteral contents -> Format.fprintf formatter "{ %a }" (Format.pp_print_list ~pp_sep:(string_sep ", ") pp) contents
      | SetComprehension (lhs, rhs) -> Format.fprintf formatter "{ %a | %a }" pp lhs pp rhs
      | Text str -> Format.fprintf formatter "%s" str

  (* an environment containing type variables for both variables and functions *)
  type env =
    | Env of (string, Typing.Var.t) Hashtbl.t * (string, Typing.Var.t list * Typing.Var.t) Hashtbl.t * env
    | Empty

  let new_env parent = Env (Hashtbl.create (module String), Hashtbl.create (module String), parent)

  let add_var env name data =
    match env with
    | Env (vars, _, _) -> Hashtbl.set vars ~key:name ~data:data
    | Empty -> raise (Typing.TypeError "Empty environment (this should not ever happen)")

  let rec get_var env (name: string) =
    match env with
    | Env (vars, _, parent) -> (
      match Hashtbl.find vars name with
      | Some x -> Some x
      | None -> get_var parent name
    )
    | Empty -> None

  let add_fn env name data =
    match env with
    | Env (_, fns, _) -> Hashtbl.set fns ~key:name ~data:data
    | Empty -> raise (Typing.TypeError "Empty environment (this should not ever happen)")

  let rec get_fn env (name: string) =
    match env with
    | Env (_, fns, parent) -> (
      match Hashtbl.find fns name with
      | Some x -> Some x
      | None -> get_fn parent name
    )
    | Empty -> None

  let type_check (math: t list) =
    let top_level = new_env Empty in

    let constraints = Typing.Constraints.create () in

    let recurse_var (name: string) = 
        match get_var top_level name with
        | Some t -> Typing.Type.Any t
        | None -> (
          let t = Typing.Var.fresh () in
          add_var top_level name t;
          Typing.Type.Any t
        )
    in

    let add_constraint c = match c with
      (* ignore trivial constraints *)
      | Typing.Constraint.Equal (Typing.Type.Number, Typing.Type.Number)
      | Equal (Typing.Type.Bool, Typing.Type.Bool) -> ()
      | Equal (Typing.Type.Any t1, Typing.Type.Any t2) when Typing.Var.equal t1 t2 -> ()
      (* keep nontrivial constraints *)
      | Equal (Any _, _) | Equal (_, Any _)
      | Equal (Set _, Set _) -> (
        Hash_set.add constraints c
      )
      (* reject impossible constraints *)
      | _ -> raise (Typing.TypeError "Type error")
    in

    let rec recurse node =
      match node with
      | Op (lhs, op, rhs) -> (match op with
        | Plus | Minus | Times | Frac -> (
          let lhs_t = recurse lhs in
          let rhs_t = recurse rhs in
          add_constraint (Equal (lhs_t, Typing.Type.Number));
          add_constraint (Equal (rhs_t, Typing.Type.Number));
          Typing.Type.Number
        )
        | Union | Inter -> (
          let t = Typing.Var.fresh () in
          let lhs_t = recurse lhs in
          let rhs_t = recurse rhs in
          add_constraint (Equal (lhs_t, Typing.Type.Set (Typing.Type.Any t)));
          add_constraint (Equal (rhs_t, Typing.Type.Set (Typing.Type.Any t)));
          Typing.Type.Set (Typing.Type.Any t)
        )
      )
      | Unary (op, lhs) -> (match op with
        | Negate -> (
          let lhs_t = recurse lhs in
          add_constraint (Equal (lhs_t, Typing.Type.Number));
          Typing.Type.Number

        )
        | Not -> (
          let lhs_t = recurse lhs in
          add_constraint (Equal (lhs_t, Typing.Type.Bool));
          Typing.Type.Bool
        )
        | Abs -> (
          let lhs_t = recurse lhs in
          add_constraint (Equal (lhs_t, Typing.Type.Number));
          Typing.Type.Number
        )
      )
      | Literal _ -> Typing.Type.Number
      | Variable name -> (
        recurse_var name
      )
      | Grouping expr -> (
        recurse expr
      )
      | Relation (lhs, rhs) -> (
        (* verify relations are valid, e.g. < only followed by = and <=, etc *)
        let verify first arr =
          (* le/leq followed only by itself or eq (same for ge/geq) *)
          (* \in and \notin only followed by set *)
          (* subset/subseteq only followed by itself or eq (same for superset/superseteq) *)
          (* same for superset/superseteq *)
          let (seen: state) = {
            le = false;
            ge = false;
            sub = false;
            sup = false; }
          in

          let rec iter prev_t arr = 
            match arr with
            | [] -> ()
            | hd :: tl -> (
              match hd with
              | (Le, _) | (Leq, _) when seen.ge -> raise (Typing.TypeError "> and >= should be followed by < or <=")
              | (Le, expr) | (Leq, expr) -> (
                seen.le <- true;
                let t = recurse expr in
                add_constraint (Equal (prev_t, Typing.Type.Number));
                add_constraint (Equal (t, Typing.Type.Number));
                iter t tl
              )
              | (Ge, _) | (Geq, _) when seen.le -> raise (Typing.TypeError "< and <= should be followed by > or >=")
              | (Ge, expr) | (Geq, expr) -> (
                seen.ge <- true;
                let t = recurse expr in
                add_constraint (Equal (prev_t, Typing.Type.Number));
                add_constraint (Equal (t, Typing.Type.Number));
                iter t tl
              )
              | (Superset, _) | (SupersetEq, _) when seen.sub -> raise (Typing.TypeError "Subset(eq) should not be followed by superset(eq)")
              | (Superset, expr) | (SupersetEq, expr) -> (
                seen.sup <- true;
                (* ensure prev_t is a set *)
                let u = Typing.Var.fresh () in
                add_constraint (Equal (prev_t, Typing.Type.Set (Typing.Type.Any u)));
                (* next t should also a set of the same type *)
                let t = recurse expr in
                add_constraint (Equal (prev_t, t));
                iter t tl
              )
              | (Subset, _) | (SubsetEq, _) when seen.sup -> raise (Typing.TypeError "Superset(eq) should not be followed by subset(eq)")
              | (Subset, expr) | (SubsetEq, expr) -> (
                seen.sub <- true;
                (* ensure prev_t is a set *)
                let u = Typing.Var.fresh () in
                add_constraint (Equal (prev_t, Typing.Type.Set (Typing.Type.Any u)));
                (* next t should also a set of the same type *)
                let t = recurse expr in
                add_constraint (Equal (prev_t, t));
                iter t tl
              )
              | (In, expr) | (NotIn, expr) -> (
                let t = recurse expr in
                add_constraint (Equal (Typing.Type.Set prev_t, t));
                iter (Typing.Type.Set prev_t) tl
              )
              | (Eq, expr) -> (
                let t = recurse expr in
                add_constraint (Equal (prev_t, t));
                iter t tl
              )
              | (_, expr) -> (
                let t = recurse expr in
                iter t tl
              )
            )
          in
          iter (recurse first) arr
        in
        verify lhs rhs;
        Typing.Type.Bool
      )
      (*
        First pass:
          Go through all declarations, find all free variables (ex: n = 2)

        Rules for scoping:
          if LHS is Apply, and all args are variables, assume to be function call.
            If any args share a name with free variables, print a warning (due to possible ambiguity)
            All args get captured as bound variables in RHS of Relation
            In RHS, if any free variables detected that have not been assigned any values, print warning (undefined variable).
            (possible extension: defining piecewise functions (could even make this the default later))

          Exists/Forall capture variables in body

          Set comprehensions can also capture variables

      *)
      | Apply (lhs, args) -> (
        let args_t, return_t = (match lhs with
          | Variable name -> (
            match get_fn top_level name with
            | Some data -> data
            | None -> (
              let args_t = List.map args ~f:(fun _ -> Typing.Var.fresh ()) in
              let return_t = Typing.Var.fresh () in
              add_fn top_level name (args_t, return_t);
              (args_t, return_t)
            )
          )
          | _ -> raise (Typing.TypeError "Cannot apply non-function")
        ) in
        List.iter (List.zip_exn args args_t) ~f:(fun (x, y) -> add_constraint (Equal (recurse x, Typing.Type.Any y)));
        Typing.Type.Any return_t
      )
      (* TODO: ignore subscripts for now *)
      | Subscript (lhs, _) ->
          recurse lhs
      (* TODO: alternative interpretations? *)
      | Superscript (lhs, rhs) -> (
          let lhs_t = recurse lhs in
          let rhs_t = recurse rhs in
          add_constraint (Equal (lhs_t, Typing.Type.Number));
          add_constraint (Equal (rhs_t, Typing.Type.Number));
          Typing.Type.Number
      )
      | Command (name, arg) -> (
          match (name, arg) with
          | ("\\mathbb", _) -> Typing.Type.Set Typing.Type.Number
          (* treat greek letters and math terms as variables *)
          | (name, _) when is_greek_letter name -> recurse_var name
          | ("\\mathit", _) | ("\\mathrm", _) -> recurse_var name
          (* assign text an arbitrary type - could be useful in future? *)
          | ("\\text", _) -> (
            let t = Typing.Var.fresh () in
            Typing.Type.Any t
          )
          | _ -> raise (Typing.TypeError "Command not yet implemented")
      )
      (* don't generate constraints, just type check insides *)
      | Forall (expr, next) -> (
        let _ = recurse expr in
        let _ = recurse next in
        Typing.Type.Bool
      )
      | Exists (expr, next) -> (
        let _ = recurse expr in
        let _ = recurse next in
        Typing.Type.Bool
      )
      | Suchthat expr -> (
        let _ = recurse expr in
        Typing.Type.Bool
      )
      | SetComprehension (lhs, rhs) -> (
        let t = recurse lhs in
        let _ = recurse rhs in
        Typing.Type.Set t
      )
      | SetLiteral lhs -> (
        let t = Typing.Var.fresh () in
        List.iter ~f:(fun expr ->
          let u = recurse expr in
          add_constraint (Equal (u, Typing.Type.Any t))
        ) lhs;
        Typing.Type.Set (Typing.Type.Any t)
      )
      (* assign text an arbitrary type - could be useful in future? *)
      | Text _ -> (
        let t = Typing.Var.fresh () in
        Typing.Type.Any t
      )
    in
    List.iter ~f:(fun x -> let _ = recurse x in ()) math;

    Format.printf "Constraints: %a\n" Typing.Constraints.pp constraints;

    let subs = Typing.unify constraints in

    match top_level with
    | Env (vars, fns, _) -> (
      Hashtbl.iteri vars ~f:(fun ~key ~data ->
        let principal_type = Typing.apply subs (Typing.Type.Any data) in
        Format.printf "val %s : %a\n" key Typing.Type.pp principal_type;
      );

      Hashtbl.iteri fns ~f:(fun ~key ~data ->
        let (args_t, ret_t) = data in
        let arg_types = List.map args_t ~f:(fun a -> Typing.apply subs (Typing.Type.Any a)) in
        let return_type  = Typing.apply subs (Typing.Type.Any ret_t) in
        Format.printf "fun %s : %a -> %a\n" key (Format.pp_print_list ~pp_sep:(fun f -> fun () -> Format.pp_print_string f " -> ") Typing.Type.pp) arg_types Typing.Type.pp return_type
      );
    );
    | Empty -> ();

    (* TODO: if variable has type Any t, warn unused *)
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

