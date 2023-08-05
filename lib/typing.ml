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
    | Sequence t -> Format.fprintf formatter "SEQ[%a]" pp t
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

  let pp formatter (subs: t) = Format.fprintf formatter "%a" (Format.pp_print_list ~pp_sep:(string_sep ", ") Constraint.pp) (Hash_set.to_list subs)

  let map ~f t =
    let new_t = create () in
    Hash_set.iter ~f:(fun a -> Hash_set.add new_t (f a)) t;
    new_t

  let first t = Hash_set.find t ~f:(fun _ -> true)

  let add constraints c = match c with
    (* ignore trivial constraints *)
    | Constraint.BoundedBy (Type.Bool, Type.Bool) -> ()
    | BoundedBy (Type.Any t1, Type.Any t2) when Var.equal t1 t2 -> ()
    | BoundedBy (Type.Number n1 , Type.Number n2) when Type.is_bounded_by n1 n2 -> ()
    (* keep nontrivial constraints *)
    | BoundedBy (Any _, _) | BoundedBy (_, Any _)
    | BoundedBy (Set _, Set _) -> (
      Hash_set.add constraints c
    )
    | BoundedBy _ -> (
      Hash_set.add constraints c
    )
    (* reject impossible constraints *)
    (* | _ -> raise (TypeError "Type error") *)
  
  let length = Hash_set.length
end

  (* (* used for addiiton and multiplication *) *)
  (* let promote t1 t2 = *)
  (*   match (t1, t2) with *)
  (*   | (Typing.Type.Number Natural, Typing.Type.Number Natural) -> Typing.Type.Number Natural *)

  (*   | (Typing.Type.Number Integer, Typing.Type.Number Natural) *)
  (*   | (Typing.Type.Number Natural, Typing.Type.Number Integer) -> Typing.Type.Number Integer *)

  (*   | (Typing.Type.Number Rational, Typing.Type.Number Natural) *)
  (*   | (Typing.Type.Number Natural, Typing.Type.Number Rational) *)
  (*   | (Typing.Type.Number Rational, Typing.Type.Number Integer) *)
  (*   | (Typing.Type.Number Integer, Typing.Type.Number Rational) -> Typing.Type.Number Rational *)

  (*   | (Typing.Type.Number Real, _) *)
  (*   | (_, Typing.Type.Number Real) -> Typing.Type.Number Real *)

  (*   | _ -> raise (Typing.TypeError "invalid arithmetic promotion (this should not happen)") *)

  (* let promote_sub t1 t2 = *)
  (*   match (t1, t2) with *)
  (*   (* we may not be able to prove if t1 > t2 *) *)
  (*   | (Typing.Type.Number Natural, Typing.Type.Number Natural) -> Typing.Type.Number Integer *)
  (*   | _ -> promote t1 t2 *)
  (*  *)

  (* let promote_frac t1 t2 = *)
  (*   match (t1, t2) with *)
  (*   | (Typing.Type.Number Natural, Typing.Type.Number Natural) *)
  (*   | (Typing.Type.Number Natural, Typing.Type.Number Integer) *)
  (*   | (Typing.Type.Number Integer, Typing.Type.Number Natural) *)
  (*   | (Typing.Type.Number Integer, Typing.Type.Number Integer) -> Typing.Type.Number Rational *)
  (*   | _ -> promote t1 t2 *)

  (* let promote_exp t1 t2 = *)
  (*   match (t1, t2) with *)
  (*   | (Typing.Type.Number Natural, Typing.Type.Number Natural) -> Typing.Type.Number Natural *)
  (*   | (Typing.Type.Number Integer, Typing.Type.Number Natural) -> Typing.Type.Number Integer *)
  (*   | (Typing.Type.Number Natural, Typing.Type.Number Integer) *)
  (*   | (Typing.Type.Number Integer, Typing.Type.Number Integer) -> Typing.Type.Number Rational *)
  (*   | (Typing.Type.Number _, Typing.Type.Number _) -> Typing.Type.Number Real *)
  (*   | _ -> raise (Typing.TypeError "invalid arithmetic promotion (this should not happen)") *)


(* maps type variables to types *)
(* the goal of type checking is to come up with substitutions to satisfy every
   constraint *)
module Substitution = struct
  type t = Type.t * Var.t (* {t / 'x}, e.g. substitute 'x with type t *)
  [@@deriving eq, sexp, hash]

  let compare a b = if equal a b then 0 else 1

  let pp formatter sub = Format.fprintf formatter "{ %a / %a }" Type.pp (fst
    sub) Var.pp (snd sub)
end

module SubstitutionList = Hash_set.Make (Substitution)

module Substitutions = struct
  type t = Substitution.t list

  let pp formatter subs = Format.fprintf formatter "%a" (Format.pp_print_list ~pp_sep:(string_sep ", ") Substitution.pp) subs
end

(* applies a type substitution to a type *)
let rec apply1 (sub: Substitution.t) (t: Type.t) =
  match t with
    | Number n -> Type.Number n
    | Bool -> Type.Bool
    | Set u -> Type.Set (apply1 sub u)
    | Tuple u -> Type.Tuple (apply1 sub u)
    | Sequence u -> Type.Sequence (apply1 sub u)
    | Any u -> if Var.equal (snd sub) u then (fst sub) else Type.Any u
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

let rec occurs_in t (u: Type.t) =
  match u with
    | Number _ -> false
    | Bool -> false
    | Bound (a, _, b) -> occurs_in t a || occurs_in t b
    | Set v -> occurs_in t v
    | Tuple v -> occurs_in t v
    | Sequence v -> occurs_in t v
    | Any v -> Var.equal t v

let unify constraints =
  let rec recurse (cs: Constraints.t) (acc: Substitutions.t) =
    if (Constraints.length cs = 0) then
      acc
    else
      let _ = Format.printf "Constraints: %a\n" Constraints.pp cs in
      (* pick out any element from the set of constraints *)
      let hd = Option.value_exn (Constraints.first cs) in
      Hash_set.remove cs hd;
      match hd with
        (* basic cases that don't require any logic *)
        (* | Equal (Type.Number n1 , Type.Number n2) when Type.equal_number n1 n2 -> recurse cs acc *)
        | BoundedBy (Bool, Bool) -> recurse cs acc
        | BoundedBy (Any t, Any u) when Var.equal t u -> recurse cs acc
        | BoundedBy (Set x, Set y) -> (
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
        | BoundedBy (Bound(a, _, b), c) -> (
          (* for now, bounds only include numeric ops *)
          Constraints.add cs (BoundedBy (Type.Number Natural, a));
          Constraints.add cs (BoundedBy (Type.Number Natural, b));
          Constraints.add cs (BoundedBy (a, c));
          Constraints.add cs (BoundedBy (b, c));
          recurse cs acc
        )
        | BoundedBy (c, Bound(a, _, b)) -> (
          (* for now, bounds only include numeric ops *)
          Constraints.add cs (BoundedBy (Type.Number Natural, a));
          Constraints.add cs (BoundedBy (Type.Number Natural, b));
          Constraints.add cs (BoundedBy (a, c));
          Constraints.add cs (BoundedBy (b, c));
          recurse cs acc
        )
        (* basic substitutions *)
        | BoundedBy (Any t, u) -> (
          if not (occurs_in t u) then
            let s = (u, t) in
            recurse (apply_cs [s] cs) (s :: acc)
          else
            raise (TypeError "Could not unify types 1")
        )
        | BoundedBy (u, Any t) -> (
          if not (occurs_in t u) then
            let s = (u, t) in
            recurse (apply_cs [s] cs) (s :: acc)
          else
            raise (TypeError "Could not unify types 2")
        )
        (* | _ -> recurse cs acc *)
        | _ -> raise (TypeError (Format.asprintf "Could not unify types 5: %a with constraints %a" Constraint.pp hd Constraints.pp cs))
  in
  let copy = Hash_set.copy constraints in (* prevent original set from being modified *)
  (* need to reverse list since it was built up in reverse in the above recursion *)
  List.rev (recurse copy [])

(* apply arithmetic promotion when possible *)
let simplify subs t =
  let t = apply subs t in
  let rec recurse t = match t with
  | Type.Bound (a, Plus, b) -> (
    match (recurse a, recurse b) with
    | (Type.Number a, Number b) -> Number (Type.widen a b)
    | _ ->  Number Real (* fallback when can't infer specific numeric type *)
  )
  | Type.Bound (a, Times, b) -> (
    match (recurse a, recurse b) with
    | (Type.Number a, Number b) -> Number (Type.widen a b)
    | _ ->  Number Real
  )
  | Type.Bound (a, Minus, b) -> (
    match (recurse a, recurse b) with
      (* a - b could be negative unless we prove a > b *)
    | (Type.Number Natural, Number Natural) -> Number Integer
    | (Type.Number a, Number b) -> Number (Type.widen a b)
    | _ ->  Number Real
  )
  | Type.Bound (a, Frac, b) -> (
    match (recurse a, recurse b) with
    | (Type.Number Natural, Number Natural)
    | (Type.Number Natural, Number Integer)
    | (Type.Number Integer, Number Natural)
    | (Type.Number Integer, Number Integer) -> Number Rational
    | (Type.Number a, Number b) -> Number (Type.widen a b)
    | _ ->  Number Real
  )
  | Type.Bound (a, Pow, b) -> (
    match (recurse a, recurse b) with
    | (Type.Number Natural, Number Natural) -> Number Natural
    | (Type.Number Integer, Number Natural) -> Number Integer
    | (Type.Number Natural, Number Integer)
    | (Type.Number Integer, Number Integer) -> Number Rational
    | (Type.Number Rational, Number Natural)
    | (Type.Number Rational, Number Integer) -> Number Rational
    | _ ->  Number Real
  )
  | Set u -> Set (recurse u)
  | Sequence u -> Sequence (recurse u)
  | _ -> t
  in
  recurse t

let test () =
  let t0 = Var.fresh () in
  let x = Type.Any t0 in
  let y = Type.Number Integer in
  let constraints = Constraints.create () in
  Constraints.add constraints (BoundedBy (x, y));
  let subs = unify constraints in
  Format.printf "%a\n" Substitutions.pp subs;
  let p = apply subs x in
  Format.printf "%a\n" Type.pp p;
  ()

