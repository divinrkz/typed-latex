open Core
open Util

(*
Type rules:
  - Literal: x must be of type T
  - Same: x must be same type as y
  - Contains: if y is has type T Set, then x must have type T (and vice versa) (for all elements in the set)
  - Function: if f(x: U) has type V, then then f has type [U] -> V (and thrice versa? - x has type U and f(x) has type V)

  - look into hoisting? since math stuff might be out of order
  - some type of subtyping would be useful - nat -> int -> rational -> real for example
  - also some type of high-order types - set<T> for example
  - would dependent types be useful? 
  - should we allow let polymorphism? e.g. should we give f(x) = x type 'a -> 'a?
    - probably not - in math, we would expect f to just be a map from one type to another, not a lambda expression
  - what about functions returning functions? ehh, that's probably fine, as long as they're defined
    - support currying? prob shouldn't, syntax doesn't really make sense

  - general algorithm: generate constraints from AST, then try to unify them

  for reference: https://cs3110.github.io/textbook/chapters/interp/inference.html#constraint-based-inference
*)

exception TypeError of string

let warn msg = Format.printf "Warning: %s\n" msg

(* a type variable *)
module Var = struct
  type t = {
    id: int
  }
  [@@deriving eq, show, sexp, hash]

  let pp formatter var = Format.fprintf formatter "'t%i" var.id

  (* instantiates a new, unique type variable each call *)
  let fresh = 
    let counter = ref 0 in
    fun () ->
      let t = { id = !counter } in
      counter := !counter + 1;
      t

  let compare a b = compare_int a.id b.id
end

module Type = struct
  type number = 
  | Natural (* includes 0 *)
  | Integer
  | Rational
  | Real
  [@@deriving eq, sexp, hash]

  let pp_number formatter n = match n with
  | Natural -> Format.fprintf formatter "NAT"
  | Integer -> Format.fprintf formatter "INT"
  | Rational -> Format.fprintf formatter "RAT"
  | Real -> Format.fprintf formatter "REAL"


  (* e.g. is t1 a subset of t2 *)
  let is_bounded_by a b =
    match (a, b) with
    | (Natural, _) -> true

    | (Integer, Natural) -> false
    | (Integer, _) -> true

    | (Rational, Natural)
    | (Rational, Integer) -> false
    | (Rational, _) -> true

    | (Real, Natural)
    | (Real, Integer)
    | (Real, Rational) -> false
    | (Real, _) -> true

let widen a b = match (a, b) with
  | (Real, _)
  | (_, Real) -> Real
  | (Rational, _)
  | (_, Rational) -> Rational
  | (Integer, _)
  | (_, Integer) -> Integer
  | (Natural, _) -> Natural


  (* type bound = *)
  (* | LessThan of Var.t *)
  (* | GreaterThan of Var.t *)
  (* | EqualTo of Var.t *)
  (* [@@deriving eq, sexp, hash] *)

  (* let pp_bound formatter n = match n with *)
  (* | LessThan t -> Format.fprintf formatter "< %a" Var.pp t *)
  (* | GreaterThan t -> Format.fprintf formatter "> %a" Var.pp t *)
  (* | EqualTo t -> Format.fprintf formatter "= %a" Var.pp t *)

  type op =
  | Plus
  | Times
  | Minus
  | Frac
  | Pow
  [@@deriving eq, sexp, hash]

  let string_of_op = function
  | Plus -> "PLUS"
  | Times -> "TIMES"
  | Minus -> "MINUS"
  | Frac -> "FRAC"
  | Pow -> "POW"

  let pp_op formatter op = Format.fprintf formatter "%s" (string_of_op op)

  type t =
    | Number of number
    | Bound of t * op * t
    | Tuple of t
    | Bool
    | Set of t
    | Sequence of t
    | Any of Var.t
  [@@deriving eq, sexp, hash]

  let rec pp formatter math = match math with
    | Number t -> Format.fprintf formatter "%a" pp_number t
    | Bound (t1, op, t2) -> Format.fprintf formatter "BOUND<%a %a %a>" pp t1 pp_op op pp t2
    | Tuple t -> Format.fprintf formatter "TUPLE<%a>" pp t
    | Bool -> Format.fprintf formatter "BOOL"
    | Set t -> Format.fprintf formatter "SET<%a>" pp t
    | Sequence t -> Format.fprintf formatter "SEQ<%a>" pp t
    | Any t -> Var.pp formatter t
end

module Constraint = struct
  type t =
    (* I realize that BoundedBy and Equal are semantically equivalent in this type system, so... *)
    (* | Equal of Type.t * Type.t (* a declaration that two types are equivalent, e.g. t1 = t2 *) *)
    | BoundedBy of Type.t * Type.t (* a declaration that one type is a subset of another, e.g. t1 <= t2 *)
  [@@deriving eq, sexp, hash]

  let compare a b = if equal a b then 0 else 1

  let pp formatter c = match c with
    (* | Equal (a, b) -> Format.fprintf formatter "[ %a = %a ]" Type.pp a Type.pp b *)
    | BoundedBy (a, b) -> Format.fprintf formatter "[ %a < %a ]" Type.pp a Type.pp b
end

module ConstraintHashSet = Hash_set.Make (Constraint)

module Constraints = struct
  include ConstraintHashSet

  (* a list of constraints along with upper/lower bounds for each variable *)
  type t = Constraint.t Hash_set.t * (Var.t, Type.t list) Hashtbl.t * (Var.t, Type.t list) Hashtbl.t

  (* let lower_bounds = Hashtbl.create (module Var) in *)
  (* let upper_bounds = Hashtbl.create (module Var) in *)

  let create () = (Hash_set.create (module Constraint), Hashtbl.create (module Var), Hashtbl.create (module Var))

  let length (x, _, _) = Hash_set.length x

  let add_bound tbl var bound =
    match Hashtbl.find tbl var with
    | Some arr -> Hashtbl.set tbl ~key:var ~data:(bound :: arr)
    | None -> Hashtbl.set tbl ~key:var ~data:[bound]

  let get_bounds tbl var =
    match Hashtbl.find tbl var with
    | Some arr -> arr
    | None -> []

  let pp formatter (subs, _, _) = Format.fprintf formatter "%a" (Format.pp_print_list ~pp_sep:(string_sep ", ") Constraint.pp) (Hash_set.to_list subs)

  let map ~f (t, u, l) =
    let new_t = Hash_set.create (module Constraint) in
    Hash_set.iter ~f:(fun a -> Hash_set.add new_t (f a)) t;
    (new_t, u, l)

  let first (t, _, _) = match Hash_set.to_list t
    with
    | hd :: _ -> Some hd
    | [] -> None

  let remove (t, _, _) e = Hash_set.strict_remove_exn t e

  let find (t, _, _) = Hash_set.find t

  let rec add (constraints, lower, upper) c =
    (* Format.printf "adding %a\n" Constraint.pp c; *)
    match c with
    (* ignore trivial constraints *)
    | Constraint.BoundedBy (x, y) when Type.equal x y -> ()
    | BoundedBy (Type.Number n1 , Type.Number n2) when Type.is_bounded_by n1 n2 -> ()
    (* keep nontrivial constraints *)
    | BoundedBy (Set x, Set y) -> (
      add (constraints, lower, upper) (BoundedBy (x, y))
    )
    | BoundedBy (Sequence x, Sequence y) -> (
      add (constraints, lower, upper) (BoundedBy (x, y))
    )
    (* TODO: add extra constraints for lower/upper bounds *)
    (* TODO: add cache to remember which have been added (since infinite loop might form) *)
    (* | BoundedBy (Any t, u) -> ( *)
    (*   (* install u as upper bound to t *) *)
    (*   add_bound upper t u; *)
    (*   (* for each lower bound of t, constrain them above by u *) *)
    (*   List.iter (get_bounds lower t) ~f:(fun x -> *)
    (*     add (constraints, lower, upper) (BoundedBy (x, u)); *)
    (*   ); *)
    (*   Hash_set.add constraints c *)
    (* ) *)
    (* | BoundedBy (u, Any t) -> ( *)
    (*   (* install u as upper bound to t *) *)
    (*   add_bound lower t u; *)
    (*   (* for each upper bound of t, constrain them below by u *) *)
    (*   List.iter (get_bounds upper t) ~f:(fun x -> *)
    (*     add (constraints, lower, upper) (BoundedBy (u, x)); *)
    (*   ); *)
    (*   Hash_set.add constraints c *)
    (* ) *)
    | BoundedBy _ -> (
      Hash_set.add constraints c
    )
    (* | _ -> raise (TypeError "Type error") *)
  
end

(* maps type variables to types *)
(* the goal of type checking is to come up with substitutions to satisfy every
   constraint *)
module Substitution = struct
  (* {t / 'x}, e.g. substitute 'x with type t *)
  (* sub (x, y) should replace all instances of y (usually a type variable) with x *)
  type t = Type.t * Type.t
  [@@deriving eq, sexp, hash]

  let compare a b = if equal a b then 0 else 1

  let pp formatter sub = Format.fprintf formatter "{ %a / %a }" Type.pp (fst sub) Type.pp (snd sub)
end

module SubstitutionList = Hash_set.Make (Substitution)

module Substitutions = struct
  type t = Substitution.t list

  let pp formatter subs = Format.fprintf formatter "%a" (Format.pp_print_list ~pp_sep:(string_sep ", ") Substitution.pp) subs

  let filter = List.filter
end

(* applies a type substitution to a type *)
let rec apply1 (sub: Substitution.t) (t: Type.t) =
  match t with
    | _ when Type.equal (snd sub) t -> fst sub
    | Number n -> Type.Number n
    | Bool -> Type.Bool
    | Set u -> Type.Set (apply1 sub u)
    | Tuple u -> Type.Tuple (apply1 sub u)
    | Sequence u -> Type.Sequence (apply1 sub u)
    | Any u -> Type.Any u
    | Bound (a, op, b) -> Type.Bound (apply1 sub a, op, apply1 sub b)

(* t (S1; S2) = (t S1) S2 *)
let rec apply (subs: Substitutions.t) (t: Type.t) =
  match subs with
    | [] -> t
    | hd :: tl -> apply tl (apply1 hd t)

(* (t1 = t2) S => t1 S = t2 S *)
let apply_c (subs: Substitutions.t) (c: Constraint.t) =
  match c with
  (* | Equal (a, b) -> Constraint.Equal (apply subs a, apply subs b) *)
  | BoundedBy (a, b) -> Constraint.BoundedBy (apply subs a, apply subs b)

let apply_cs (subs: Substitutions.t) (cs: Constraints.t) = Constraints.map ~f:(apply_c subs) cs

let exists = function
  | Some _ -> true
  | None -> false

let rec simplifiable (t: Type.t) =
  match t with
  | Number _ -> true
  | Bool -> true
  | Bound (Number _, _, Number _) -> true
  | Bound (a, _, b) -> simplifiable a && simplifiable b
  | Set v -> simplifiable v
  | Tuple v -> simplifiable v
  | Sequence v -> simplifiable v
  | Any _ -> false

(* returns true if t occurs within u *)
let rec occurs_in (t: Type.t) (u: Type.t) =
  match u with
    | _ when Type.equal t u -> true
    | Number _ -> false
    | Bool -> false
    | Bound (a, _, b) -> occurs_in t a || occurs_in t b
    | Set v -> occurs_in t v
    | Tuple v -> occurs_in t v
    | Sequence v -> occurs_in t v
    | Any _ -> Type.equal t u

let unify (constraints, lower, upper) =
  let rec recurse (cs: Constraints.t) (acc: Substitutions.t) =
    if (Constraints.length cs = 0) then
      acc
    else
      (* let _ = Format.printf "Constraints: %a\n" Constraints.pp cs in *)
      (* pick out any element from the set of constraints *)
      let hd = Option.value_exn (Constraints.first cs) in
      Format.printf "hd: %a\n" Constraint.pp hd;
      Constraints.remove cs hd;
      let () = match Constraints.find cs ~f:(fun x -> Constraint.equal x hd) with
        | Some _ -> Format.printf "wtf\n"
        | None -> ()
      in
      match hd with
      (* basic cases that don't require any logic *)
      (* | Equal (Type.Number n1 , Type.Number n2) when Type.equal_number n1 n2 -> recurse cs acc *)
      | BoundedBy (x, y) when Type.equal x y -> recurse cs acc

      | BoundedBy (Bool, Bool) -> recurse cs acc
      | BoundedBy (Any t, Any u) when Var.equal t u -> recurse cs acc
      | BoundedBy (Set x, Set y) -> (
        Constraints.add cs (BoundedBy (x, y));
        recurse cs acc
      )
      | BoundedBy (Tuple x, y) -> (
        Constraints.add cs (BoundedBy (x, y));
        recurse cs acc
      )
      | BoundedBy (x, Tuple y) -> (
        Constraints.add cs (BoundedBy (x, y));
        recurse cs acc
      )
      | BoundedBy (Sequence x, Sequence y) -> (
        Constraints.add cs (BoundedBy (x, y));
        recurse cs acc
      )
      (* arithmetic and type promotion *)
      | BoundedBy (Type.Number n1 , Type.Number n2) when Type.is_bounded_by n1 n2 -> recurse cs acc
      (* these bounds are weaker, but it's probably the best we can reasonably do without backtracking *)
      (* ex: if we have \frac{a}{b} = \sqrt{2}, we don't know if a is real, b is real, or both.*)
      (* we will just assume that both are real*)
      (* TODO: Ok actually, what if we just disallow reverse inference altogether? *)
      (* are there any cases where reverse inference would actually be useful? *)
      (* | BoundedBy (Bound (a, op, b), c) when not (simplifiable (Bound (a, op, b))) -> ( *)
      (*   (* only if Bound cannot be simplified *) *)
      (*   if not (occurs_in (Bound (a, op, b)) c) then *)
      (*     let s = (c, Type.Bound (a, op, b)) in *)
      (*     recurse (apply_cs [s] cs) (s :: acc) *)
      (*   else *)
      (*     raise (TypeError "Could not unify types 2") *)
      (* ) *)
      (* | BoundedBy (c, Bound (a, op, b)) when not (simplifiable (Bound (a, op, b))) -> ( *)
      (*   if not (occurs_in (Bound (a, op, b)) c) then *)
      (*     let s = (c, Type.Bound (a, op, b)) in *)
      (*     recurse (apply_cs [s] cs) (s :: acc) *)
      (*   else *)
      (*     raise (TypeError "Could not unify types 2") *)
      (* ) *)
      (* | BoundedBy (Bound(a, _, b), c) -> ( *)
      (*   Constraints.add cs (BoundedBy (a, c)); *)
      (*   Constraints.add cs (BoundedBy (b, c)); *)
      (*   recurse cs acc *)
      (* ) *)
      (* | BoundedBy (c, Bound(a, _, b)) -> ( *)
      (*   Format.printf "Solving %a < Bound<%a, %a>\n" Type.pp c Type.pp a Type.pp b; *)
      (*   Constraints.add cs (BoundedBy (c, a)); *)
      (*   Constraints.add cs (BoundedBy (c, b)); *)
      (*   recurse cs acc *)
      (* ) *)
      | BoundedBy (Any t, u) -> (
        (* TODO: need to check variable bounds *)
        (* Format.printf "Replacing %a with %a (>)\n" Var.pp t Type.pp u; *)
        if not (occurs_in (Any t) u) then
          (* let hi = Constraints.get_bounds upper t in *)
          (* Format.printf "Current upper bounds: [%a]\n" (Format.pp_print_list ~pp_sep:(string_sep ", ") Type.pp) hi; *)
          (* let lo = Constraints.get_bounds lower t in *)
          (* Format.printf "Current lower bounds: [%a]\n" (Format.pp_print_list ~pp_sep:(string_sep ", ") Type.pp) lo; *)
          let s = (u, Type.Any t) in
          (* TODO: also apply substitutions within lower/upper bounds? *)
          recurse (apply_cs [s] cs) (s :: acc)
        else
          (* raise (TypeError "Could not unify types 1") *)
          recurse cs acc
      )
      | BoundedBy (u, Any t) -> (
        (* Format.printf "Replacing %a with %a (<)\n" Var.pp t Type.pp u; *)
        (* let hi = Constraints.get_bounds upper t in *)
        (* Format.printf "Current upper bounds: [%a]\n" (Format.pp_print_list ~pp_sep:(string_sep ", ") Type.pp) hi; *)
        (* let lo = Constraints.get_bounds lower t in *)
        (* Format.printf "Current lower bounds: [%a]\n" (Format.pp_print_list ~pp_sep:(string_sep ", ") Type.pp) lo; *)
        if not (occurs_in (Any t) u) then
          let s = (u, Type.Any t) in
          recurse (apply_cs [s] cs) (s :: acc)
        else
          (* raise (TypeError "Could not unify types 2") *)
          recurse cs acc
      )
      | _ -> recurse cs acc
      (* | _ -> raise (TypeError (Format.asprintf "Could not unify types 5: %a with constraints %a" Constraint.pp hd Constraints.pp cs)) *)
  in
  (* Format.printf "Lower: %a\n" (pp_hashtbl ~pp_key:Var.pp ~pp_data:(Format.pp_print_list ~pp_sep:(string_sep ", ") Type.pp)) lower; *)
  (* Format.printf "Upper: %a\n" (pp_hashtbl ~pp_key:Var.pp ~pp_data:(Format.pp_print_list ~pp_sep:(string_sep ", ") Type.pp)) upper; *)
  let copy = Constraints.map ~f:id (constraints, lower, upper) in (* prevent original set from being modified *)
  (* need to reverse list since it was built up in reverse in the above recursion *)
  List.rev (recurse copy [])

(* apply arithmetic promotion when possible *)
let simplify subs t =
  let t = apply subs t in
  (* Format.printf "Simplifying %a\n" Type.pp t; *)
  let rec recurse t = match t with
  | Type.Bound (a, Plus, b) -> (
    match (recurse a, recurse b) with
    | (Type.Number a, Number b) -> Number (Type.widen a b)
    | _ -> raise (TypeError (Format.asprintf "Could not infer numeric type for %a + %a" Type.pp a Type.pp b))
  )
  | Type.Bound (a, Times, b) -> (
    match (recurse a, recurse b) with
    | (Type.Number a, Number b) -> Number (Type.widen a b)
    | _ -> raise (TypeError (Format.asprintf "Could not infer numeric type for %a * %a" Type.pp a Type.pp b))
  )
  | Type.Bound (a, Minus, b) -> (
    match (recurse a, recurse b) with
      (* a - b could be negative unless we prove a > b *)
    | (Type.Number Natural, Number Natural) -> Number Integer
    | (Type.Number a, Number b) -> Number (Type.widen a b)
    | _ -> raise (TypeError (Format.asprintf "Could not infer numeric type for %a - %a" Type.pp a Type.pp b))
  )
  | Type.Bound (a, Frac, b) -> (
    match (recurse a, recurse b) with
    | (Type.Number Natural, Number Natural)
    | (Type.Number Natural, Number Integer)
    | (Type.Number Integer, Number Natural)
    | (Type.Number Integer, Number Integer) -> Number Rational
    | (Type.Number a, Number b) -> Number (Type.widen a b)
    | _ -> raise (TypeError (Format.asprintf "Could not infer numeric type for %a / %a" Type.pp a Type.pp b))
  )
  | Type.Bound (a, Pow, b) -> (
    match (recurse a, recurse b) with
    | (Type.Number Natural, Number Natural) -> Number Natural
    | (Type.Number Integer, Number Natural) -> Number Integer
    | (Type.Number Natural, Number Integer)
    | (Type.Number Integer, Number Integer) -> Number Rational
    | (Type.Number Rational, Number Natural)
    | (Type.Number Rational, Number Integer) -> Number Rational
    | _ -> raise (TypeError (Format.asprintf "Could not infer numeric type for %a ^ %a" Type.pp a Type.pp b))
  )
  | Set u -> Set (recurse u)
  | Sequence u -> Sequence (recurse u)
  | Any _ -> (
      (* Format.printf "Inferring %a to be Real\n" Var.pp t; *)
      Number Real
    )
  | _ -> t
  in
  recurse t
