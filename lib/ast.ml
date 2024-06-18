open Core
open Util
open Typing

module rec Math : sig
  type operator =
    | Plus
    | Minus
    | Times
    | Frac
    | Union
    | Inter

  type unary =
    | Negate
    | Not
    | Abs

  type logic_op =
    | And
    | Or

  type logic =
    | Implies
    | Iff

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
    | Equiv of t
  and t =
    | Op of t * operator * t
    | Unary of unary * t
    | Relation of t * (relation * t) list (* LHS (= RHS)+ *)
    | LogicOp of t * logic_op * t
    | Logic of t * (logic * t) list
    | IntLiteral of int
    | FloatLiteral of float
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
    | Summation of t * t * t
    | Tuple of t list

  val pp: Format.formatter -> t -> unit

  val type_check: t list -> unit

  val string_of_logic: logic -> string

end = struct
  type operator =
    | Plus
    | Minus
    | Times
    | Frac
    | Union
    | Inter

  type unary =
    | Negate
    | Not
    | Abs

  type logic_op =
    | And
    | Or

  type logic =
    | Implies
    | Iff

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
    | Equiv of t
  and t =
    | Op of t * operator * t
    | Unary of unary * t
    | Relation of t * (relation * t) list
    | LogicOp of t * logic_op * t
    | Logic of t * (logic * t) list
    | IntLiteral of int
    | FloatLiteral of float
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
    | Summation of t * t * t
    | Tuple of t list

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

  let string_of_logic_op = function
    | And -> "AND"
    | Or -> "OR"

  let string_of_logic = function
    | Implies -> "IMPLIES"
    | Iff -> "IFF"

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

  let rec pp_relation formatter rel = match rel with
    | Le -> Format.fprintf formatter "<"
    | Leq -> Format.fprintf formatter "<="
    | Ge -> Format.fprintf formatter ">"
    | Geq -> Format.fprintf formatter ">="
    | Eq -> Format.fprintf formatter "="
    | In -> Format.fprintf formatter "IN"
    | NotIn -> Format.fprintf formatter "NOTIN"
    | Subset -> Format.fprintf formatter "SUBSET"
    | Superset -> Format.fprintf formatter "SUPERSET"
    | SubsetEq -> Format.fprintf formatter "SUBSETEQ"
    | SupersetEq -> Format.fprintf formatter "SUPERSETEQ"
    | Equiv x -> Format.fprintf formatter "EQUIV_%a" pp x
  and pp formatter math = match math with
    | Op (lhs, op, rhs) -> Format.fprintf formatter "(%s %a %a)" (string_of_operator op) pp lhs pp rhs
    | LogicOp (lhs, op, rhs) -> Format.fprintf formatter "(%s %a %a)" (string_of_logic_op op) pp lhs pp rhs
    | Logic (lhs, rhs) -> (
      let pp_pair = fun formatter -> fun (rel, t) -> Format.fprintf formatter "%s %a" (string_of_logic rel) pp t in
      Format.fprintf formatter "(%a %a)" pp lhs (Format.pp_print_list ~pp_sep:(string_sep " ") pp_pair) (List.rev rhs);
    )
    | Unary (op, lhs) -> Format.fprintf formatter "(%s %a)" (string_of_unary op) pp lhs
    | IntLiteral num -> Format.fprintf formatter "%i" num
    | FloatLiteral num -> Format.fprintf formatter "%f" num
    | Variable var -> Format.fprintf formatter "%s" var
    | Grouping expr -> Format.fprintf formatter "%a" pp expr
    | Relation (lhs, rhs) -> (
      let pp_pair = fun formatter -> fun (rel, t) -> Format.fprintf formatter "%a %a" pp_relation rel pp t in
      Format.fprintf formatter "(%a %a)" pp lhs (Format.pp_print_list ~pp_sep:(string_sep " ") pp_pair) (List.rev rhs);
    )
    | Apply (lhs, rhs) -> Format.fprintf formatter "(%a %a)"  pp lhs (Format.pp_print_list ~pp_sep:(string_sep " ") pp) rhs
    | Subscript (lhs, rhs) -> Format.fprintf formatter "%a_%a"  pp lhs pp rhs
    | Superscript (lhs, rhs) -> Format.fprintf formatter "(^ %a %a)"  pp lhs pp rhs
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
    | Summation (low, high, body) -> Format.fprintf formatter "(SUM_%a^%a %a)" pp low pp high pp body
    | Tuple contents -> Format.fprintf formatter "(%a)" (Format.pp_print_list ~pp_sep:(string_sep ", ") pp) contents

  (* an environment containing type variables for variables *)
  type env =
    | Env of (string, Typing.Var.t) Hashtbl.t * env
    | Empty

  let new_env parent = Env (Hashtbl.create (module String), parent)

  let add_var env name data =
    match env with
    | Env (vars, _) -> Hashtbl.set vars ~key:name ~data:data
    | Empty -> raise (TypeError "Empty environment (this should not ever happen)")

  let rec get_var env (name: string) =
    match env with
    | Env (vars, parent) -> (
      match Hashtbl.find vars name with
      | Some x -> Some x
      | None -> get_var parent name
    )
    | Empty -> None

  type fns = (string, Typing.Var.t list * Typing.Var.t) Hashtbl.t

  let add_fn (env: fns) name data = Hashtbl.set env ~key:name ~data:data

  let get_fn (env: fns) (name: string) = Hashtbl.find env name

  let type_check (math: t list) =
    let top_level = new_env Empty in
    let functions = Hashtbl.create (module String) in

    let constraints = Typing.Constraints.create () in

    let recurse_var env (name: string) = 
        match get_var env name with
        | Some t -> Type.Any t
        | None -> (
          let t = Typing.Var.fresh () in
          add_var top_level name t; (* free variables go straight to the top level environment *)
          Type.Any t
        )
    in

    let add_constraint = Typing.Constraints.add constraints in

    let rec captured_vars node =
      match node with
      | Variable _ -> [node]
      | Command (name, _) when is_greek_letter name -> [node]
      | LogicOp (lhs, And, rhs) -> (
          List.append (captured_vars lhs) (captured_vars rhs)
        )
      | Relation (lhs, _) -> (
          captured_vars lhs
        )
      | _ -> []
    in

    let capture parent node =
      let vars = captured_vars node in
      if List.length vars = 0 then
        parent
      else
        let child = new_env parent in
        List.iter vars ~f:(fun v -> 
          match v with
          | Variable name -> add_var child name (Typing.Var.fresh ())
          | Command (name, _) -> add_var child name (Typing.Var.fresh ())
          | _ -> raise (TypeError "This should not happen")
        );
        child
    in

    (* TODO: check if all the bounds are correct. Order matters now... *)
    let rec recurse_fn env name args =
      match get_fn functions name with
      | Some (args_t, return_t) -> (
          if List.length args <> List.length args_t then
            raise (TypeError (Format.sprintf "Conflicting declarations for function %s" name))
          else
            List.iter (List.zip_exn args args_t) ~f:(fun (x, y) -> add_constraint (recurse env x, Type.Any y));
            (args_t, return_t)
        )
      | None -> (
        let args_t = List.map args ~f:(fun _ -> Typing.Var.fresh ()) in
        let return_t = Typing.Var.fresh () in
        add_fn functions name (args_t, return_t);
        (args_t, return_t)
      )
    and recurse (env: env) node =
      match node with
      | LogicOp (lhs, _, rhs) -> (
        let lhs_t = recurse env lhs in
        let rhs_t = recurse env rhs in
        add_constraint (lhs_t, Type.Bool);
        add_constraint (rhs_t, Type.Bool);
        Type.Bool;
      )
      | Logic (lhs, rhs) -> (
        let lhs_t = recurse env lhs in
        List.iter (List.rev rhs) ~f:(fun x ->
          let rhs_t = recurse env (snd x) in
          add_constraint (rhs_t, Type.Bool);
        );
        add_constraint (lhs_t, Type.Bool);
        Type.Bool;
      )
      | Op (lhs, op, rhs) -> (match op with
        | Plus -> (
          let t = Typing.Var.fresh () in
          let lhs_t = recurse env lhs in
          let rhs_t = recurse env rhs in
          add_constraint (Type.Any t, (Bound (lhs_t, Plus, rhs_t)));
          Type.Any t
        )
        | Times -> (
          let t = Typing.Var.fresh () in
          let lhs_t = recurse env lhs in
          let rhs_t = recurse env rhs in
          add_constraint (Type.Any t, (Bound (lhs_t, Times, rhs_t)));
          Type.Any t
        )
        | Minus -> (
          let t = Typing.Var.fresh () in
          let lhs_t = recurse env lhs in
          let rhs_t = recurse env rhs in
          add_constraint (Type.Any t, (Bound (lhs_t, Minus, rhs_t)));
          Type.Any t
        )
        | Frac -> (
          let t = Typing.Var.fresh () in
          let lhs_t = recurse env lhs in
          let rhs_t = recurse env rhs in
          add_constraint (Type.Any t, (Bound (lhs_t, Frac, rhs_t)));
          Type.Any t
        )
        | Union | Inter -> (
          let t = Typing.Var.fresh () in
          let lhs_t = recurse env lhs in
          let rhs_t = recurse env rhs in
          add_constraint (lhs_t, Type.Set (Type.Any t));
          add_constraint (rhs_t, Type.Set (Type.Any t));
          Type.Set (Type.Any t)
        )
      )
      | Unary (op, lhs) -> (match op with
        | Negate -> (
          let t = Typing.Var.fresh () in
          let lhs_t = recurse env lhs in
          (* same type constraints as minus, since -x is 0 - x (e.g. negative of a natural is an integer)*)
          add_constraint (Type.Any t, (Bound (Number Natural, Minus, lhs_t)));
          Type.Any t

        )
        | Not -> (
          let lhs_t = recurse env lhs in
          add_constraint (lhs_t, Type.Bool);
          Type.Bool
        )
        | Abs -> (
          let lhs_t = recurse env lhs in
          add_constraint (lhs_t, Type.Number Real);
          lhs_t
        )
        (* | Sqrt -> ( *)
        (*   let t = Typing.Var.fresh () in *)
        (*   let lhs_t = recurse env lhs in *)
        (*   (* ensure that contents are numeric, but result is always real *) *)
        (*   add_constraint (lhs_t, Type.Number Real); *)
        (*   add_constraint (Type.Any t, Type.Number Real); *)
        (*   Type.Any t *)
        (* ) *)
      )
      | IntLiteral _ -> Type.Number Natural
      | FloatLiteral _ -> Type.Number Real
      | Variable name -> (
        recurse_var env name
      )
      | Grouping expr -> (
        recurse env expr
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
              | (Le, _) | (Leq, _) when seen.ge -> raise (TypeError "> and >= should be followed by < or <=")
              | (Le, expr) | (Leq, expr) -> (
                seen.le <- true;
                let t = recurse env expr in
                (* TODO: is this correct? *)
                (* add_constraint (prev_t, Type.Number Natural); *)
                (* add_constraint (t, Type.Number Natural); *)
                iter t tl
              )
              | (Ge, _) | (Geq, _) when seen.le -> raise (TypeError "< and <= should be followed by > or >=")
              | (Ge, expr) | (Geq, expr) -> (
                seen.ge <- true;
                let t = recurse env expr in
                (* add_constraint (prev_t, Type.Number Natural); *)
                (* add_constraint (t, Type.Number Natural); *)
                iter t tl
              )
              | (Superset, _) | (SupersetEq, _) when seen.sub -> raise (TypeError "Subset(eq) should not be followed by superset(eq)")
              | (Superset, expr) | (SupersetEq, expr) -> (
                seen.sup <- true;
                (* ensure prev_t is a set *)
                let u = Typing.Var.fresh () in
                add_constraint (prev_t, Type.Set (Type.Any u));
                (* next t should be bounded by the previous t *)
                let t = recurse env expr in
                add_constraint (t, prev_t);
                iter t tl
              )
              | (Subset, _) | (SubsetEq, _) when seen.sup -> raise (TypeError "Superset(eq) should not be followed by subset(eq)")
              | (Subset, expr) | (SubsetEq, expr) -> (
                seen.sub <- true;
                (* ensure prev_t is a set *)
                let u = Typing.Var.fresh () in
                add_constraint (prev_t, Type.Set (Type.Any u));
                (* the previous t should be bounded by the next t *)
                let t = recurse env expr in
                add_constraint (prev_t, t);
                iter t tl
              )
              | (In, expr) | (NotIn, expr) -> (
                let t = recurse env expr in
                (* ensure t is a set *)
                let u = Typing.Var.fresh () in
                add_constraint (t, Type.Set (Type.Any u));
                add_constraint (prev_t, Type.Any u);
                iter t tl
              )
              | (Eq, expr) -> (
                let t = recurse env expr in
                add_constraint (prev_t, t);
                add_constraint (t, prev_t);
                iter t tl
              )
              | (Equiv m, expr) -> (
                let t = recurse env expr in
                let m_t = recurse env m in
                add_constraint (m_t, Type.Number Natural);
                add_constraint (prev_t, Type.Number Integer);
                add_constraint (t, Type.Number Integer);
                iter t tl
              )
            )
          in
          iter (recurse env first) arr
        in
        verify lhs (List.rev rhs);
        Type.Bool
      )
      | Apply (lhs, [Tuple args]) -> (
        let args_t, return_t = (match lhs with
          | Variable name -> recurse_fn env name args
          | _ -> raise (TypeError "Cannot apply non-function")
        ) in
        List.iter (List.zip_exn args args_t) ~f:(fun (x, y) -> add_constraint (recurse env x, Type.Any y));
        Type.Any return_t
      )
      | Apply (lhs, args) -> (
        let args_t, return_t = (match lhs with
          | Variable name -> recurse_fn env name args
          | _ -> raise (TypeError "Cannot apply non-function")
        ) in
        List.iter (List.zip_exn args args_t) ~f:(fun (x, y) -> add_constraint (recurse env x, Type.Any y));
        Type.Any return_t
      )
      | Subscript (lhs, rhs) -> (
        let t = Typing.Var.fresh () in
        let lhs_t = recurse env lhs in
        let rhs_t = recurse env rhs in
        add_constraint (rhs_t, Type.Number Natural);
        add_constraint (lhs_t, Type.Sequence (Type.Any t));
        Type.Any t
      )
      (* TODO: alternative interpretations? *)
      | Superscript (lhs, rhs) -> (
          let t = Typing.Var.fresh () in
          let lhs_t = recurse env lhs in
          let rhs_t = recurse env rhs in
          add_constraint (Type.Any t, (Bound (lhs_t, Pow, rhs_t)));
          Type.Any t
      )
      | Command (name, arg) -> (
          match (name, arg) with
          (* this is kind of a hack, maybe fix later? *)
          | ("\\mathbb", Some (Variable "N")) -> Type.Set (Type.Number Natural)
          | ("\\mathbb", Some (Variable "Z")) -> Type.Set (Type.Number Integer)
          | ("\\mathbb", Some (Variable "Q")) -> Type.Set (Type.Number Rational)
          | ("\\mathbb", Some (Variable "R")) -> Type.Set (Type.Number Real)
          (* treat greek letters and math terms as variables *)
          | (name, _) when is_greek_letter name -> recurse_var env name
          | ("\\mathit", _) | ("\\mathrm", _) -> recurse_var env name
          (* assign text an arbitrary type - could be useful in future? *)
          | ("\\text", _) -> (
            let t = Typing.Var.fresh () in
            Type.Any t
          )
          | ("\\sqrt", Some lhs) -> (
            let t = Typing.Var.fresh () in
            let lhs_t = recurse env lhs in
            (* ensure that contents are numeric, but result is always real *)
            add_constraint (lhs_t, Type.Number Real);
            add_constraint (Type.Any t, Type.Number Real);
            Type.Any t
          )
          | ("\\sqrt", None) -> raise (TypeError "sqrt not provided with an argument")
          | _ -> Type.Any (Typing.Var.fresh ())
          (* | _ -> raise (TypeError "Command not yet implemented") *)
      )
      | Forall (expr, next) -> (
        let child = capture env expr in
        let _ = recurse child expr in
        let _ = recurse child next in
        Type.Bool
      )
      | Exists (expr, next) -> (
        let child = capture env expr in
        let _ = recurse child expr in
        let _ = recurse child next in
        Type.Bool
      )
      | Suchthat expr -> (
        let _ = recurse env expr in
        Type.Bool
      )
      | Summation (low, high, body) -> (
        let child = capture env low in
        let _ = recurse child low in
        let _ = recurse child high in
        let t = recurse child body in
        t
      )
      | SetComprehension (lhs, rhs) -> (
        let child = capture env rhs in
        let _ = recurse child rhs in
        let t = recurse child lhs in
        Type.Set t
      )
      | SetLiteral lhs -> (
        let t = Typing.Var.fresh () in
        List.iter ~f:(fun expr ->
          let u = recurse env expr in
          add_constraint (u, Type.Any t)
        ) lhs;
        Type.Set (Type.Any t)
      )
      | Tuple lhs -> (
        let t = Typing.Var.fresh () in
        List.iter ~f:(fun expr ->
          let u = recurse env expr in
          add_constraint (u, Type.Any t)
        ) lhs;
        Type.Tuple (Type.Any t)
      )
      (* assign text an arbitrary type - could be useful in future? *)
      | Text _ -> (
        let t = Typing.Var.fresh () in
        Type.Any t
      )
    in

    let rec all_vars arr =
      match arr with
      | [] -> true
      | hd :: tl -> (match hd with
          | Variable _ -> all_vars tl
          | _ -> false
        )
    in

    let recurse0 env node =
      match node with
      | Relation (Apply (Variable name, args), (Eq, _) :: _) -> (
            if all_vars args then
              (* create a new environment that captures the variables *)
              let child = new_env env in
              List.iter args ~f:(fun arg -> match arg with
                | Variable a -> add_var child a (Typing.Var.fresh ())
                | _ -> raise (TypeError "Invalid function declaration (this should not happen)")
              );
              let _ = recurse_fn child name args in
              recurse child node
            else
              recurse env node
      )
      | _ -> recurse env node
    in

    List.iter ~f:(fun x -> let _ = recurse0 top_level x in ()) math;

    (* Format.printf "Constraints: %a\n" Typing.Constraints.pp constraints; *)

    let subs = Typing.unify constraints in

    (* Format.printf "Subs: %a\n" Typing.Substitutions.pp subs; *)

    (* everything in the top level environment is a free variable *)
    match top_level with
    | Env (vars, _) -> (
      Hashtbl.iteri vars ~f:(fun ~key ~data ->
        let principal_type = Typing.simplify ~final:true (Typing.apply subs (Type.Any data)) in
        Format.printf "val (%a) %s : %a\n" Typing.Var.pp data key Type.pp principal_type;
      );

      Hashtbl.iteri functions ~f:(fun ~key ~data ->
        let (args_t, ret_t) = data in
        let arg_types = List.map args_t ~f:(fun a -> Typing.simplify ~final:true (Typing.apply subs (Type.Any a))) in
        let return_type  = Typing.simplify ~final:true (Typing.apply subs (Type.Any ret_t)) in
        Format.printf "fun %s : %a -> %a\n" key (Format.pp_print_list ~pp_sep:(string_sep " -> ") Type.pp) arg_types Type.pp return_type
      );
    );
    | Empty -> ();

    (* TODO: if variable has type Any t, warn unused *)
    ()

end
and Statement: sig
    type t =
    | Fact of Math.t
    | Assumption of Math.t
    | Definition of Math.t

    (* let all_statements = [] *)
    (* TODO: constant propagation *)
    (* what if mutually recursive statements? *)
    (*
      n = 2 * m
      m = 2 * n
      # n -> m -> n

      P(n) = 2n = 4 < 3n simplifies to:
        P(n) = 2n
        P(n) = 4
        2n = 4 (where n is a captured variable, not free)
        P(n) < 3n
        2n < 3n
        4 < 3n
    *)
    (* 
       While walking the tree, generate a dependency graph linking nodes to free variables and (constant) functions.
       Then, after walking, go through all the constant nodes, evaluate them, and evaluate the nodes that depend on them recursively as far as possible.
       Check that each node N is visited at most indegree(N) times.
       Also, we only need to assign constant variables to functions and variables (doesn't rly make sense to replace AST nodes, since user won't see them).

       I feel like this needs to be done after the type checking stuff - there's a lot that needs to be done.
       While type checking, generate a list of statements (like edge list). Then, solve edge list to evaluate constants, etc, and print out assumptions.
          *)
end = struct
    type t =
    | Fact of Math.t
    | Assumption of Math.t
    | Definition of Math.t
end

module Node = struct
  type 'node t = {
    pos: Lexing.position;
    value: 'node;
  }
end

module rec Environment : sig
  (* name * arglist * contents *)
  type t = string * Latex.t list * Latex.t list

  val name: Environment.t -> string
  val args: Environment.t -> Latex.t list
  val body: Environment.t -> Latex.t list
end = struct
  type t = string * Latex.t list * Latex.t list

  let name (a, _, _) = a
  let args (_, a, _) = a
  let body (_, _, a) = a
end
and Latex: sig
  type latex =
    | Word of string
    | Whitespace
    | Newline
    | Command of string * t list
    | Environment of Environment.t
    | Mathmode of string (* encompasses equations, formula, and display mode *)
    | Multiline of string (* encompasses only align* (for now) *)
    | Latex of t list
  and t = latex Node.t

  val pp: Format.formatter -> t -> unit
  val pp_list: Format.formatter -> t list -> unit
  val get_all_math: t -> latex list
end = struct
  type latex =
    | Word of string
    | Whitespace
    | Newline
    | Command of string * t list
    | Environment of Environment.t
    | Mathmode of string
    | Multiline of string
    | Latex of t list
  and t = latex Node.t

  let rec pp_environment formatter (env: Environment.t) =
    let contents = Format.pp_print_list pp in
    Format.fprintf formatter "(%s %a)" (Environment.name env) contents (Environment.body env)
  and pp_mathmode formatter math = Format.fprintf formatter "MATH[%s]" math
  and pp formatter (latex: t) = match latex with
    | {pos = _; value = Word word} -> Format.fprintf formatter "%s" word
    | {pos = _; value = Whitespace} -> Format.fprintf formatter " "
    | {pos = _; value = Newline} -> Format.fprintf formatter "\n"
    | {pos = _; value = Environment env} -> pp_environment formatter env
    | {pos = _; value = Mathmode math} -> pp_mathmode formatter math
    | {pos = _; value = Multiline math} -> pp_mathmode formatter math
    | {pos = _; value = Command (name, args)} -> Format.fprintf formatter "(\\%s %a)" name (Format.pp_print_list ~pp_sep:(string_sep " ") pp) args
    | {pos = _; value = Latex contents} -> Format.fprintf formatter "(%a)" (Format.pp_print_list pp) contents

  let pp_list formatter asts = Format.fprintf formatter "%a\n" (Format.pp_print_list ~pp_sep:(string_sep "\n") pp) asts

  (* let pp_position formatter (pos: Lexing.position) = Format.fprintf formatter "%s:%i:%i" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) *)

  let get_all_math node =
    let acc = ref [] in
    let rec recurse acc (node: t) = match node with
    | {pos = _; value = Word _} -> ()
    | {pos = _; value = Whitespace } -> ()
    | {pos = _; value = Newline } -> ()
    | {pos = _; value = Environment (_, _, contents)} -> List.iter ~f:(recurse acc) contents
    | {pos = _; value = Mathmode _ as math} -> (
        acc := math :: !acc;
      )
    | {pos = _; value = Multiline _ as math} -> (
        acc := math :: !acc;
      )
    | {pos = _; value = Command (_, args)} -> List.iter ~f:(recurse acc) args
    | {pos = _; value = Latex contents} -> List.iter ~f:(recurse acc) contents
    in
    recurse acc node;
    List.rev !acc


end

