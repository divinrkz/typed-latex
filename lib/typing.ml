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

(* a type variable *)
module Var = struct
  type t = {
    id: int
  }
  [@@deriving eq, show, sexp, hash, ord]

  let pp formatter var = Format.fprintf formatter "'t%i" var.id

  (* instantiates a new, unique type variable each call (by incrementing a counter) *)
  let fresh = 
    let counter = ref 0 in
    fun () ->
      let t = { id = !counter } in
      counter := !counter + 1;
      t
end

(* the type of an object *)
module Type = struct
  type number = 
  | Natural (* includes 0 *)
  | Integer
  | Rational
  | Real
  [@@deriving eq, show, sexp, hash, ord]

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

  type op =
  | Plus
  | Times
  | Minus
  | Frac
  | Pow
  [@@deriving eq, show, sexp, hash, ord]

  let pp_op formatter op = match op with
  | Plus -> Format.fprintf formatter "PLUS"
  | Times -> Format.fprintf formatter "TIMES"
  | Minus -> Format.fprintf formatter "MINUS"
  | Frac -> Format.fprintf formatter "FRAC"
  | Pow -> Format.fprintf formatter "POW"

  type t =
    | Bool
    | Number of number
    | Tuple of t
    | Set of t
    | Sequence of t
    | Any of Var.t
    | Bound of t * op * t (* used internally to simplify for numeric types *)
  [@@deriving eq, show, sexp, hash, ord]

  let rec pp formatter math = match math with
    | Bool -> Format.fprintf formatter "BOOL"
    | Number t -> Format.fprintf formatter "%a" pp_number t
    | Tuple t -> Format.fprintf formatter "TUPLE<%a>" pp t
    | Set t -> Format.fprintf formatter "SET<%a>" pp t
    | Sequence t -> Format.fprintf formatter "SEQ<%a>" pp t
    | Any t -> Var.pp formatter t
    | Bound (t1, op, t2) -> Format.fprintf formatter "BOUND<%a %a %a>" pp t1 pp_op op pp t2
end

module Constraint = struct
  (* TODO: is this = or <: *)
  (* a declaration that one type is bounded by another, e.g. t1 <: t2 *)
  type t = Type.t * Type.t 
  [@@deriving eq, show, sexp, hash, ord]

  let pp formatter (a, b) = Format.fprintf formatter "[ %a < %a ]" Type.pp a Type.pp b
end

module ConstraintHashSet = Hash_set.Make (Constraint)

module Constraints = struct
  include ConstraintHashSet

  let create () = Hash_set.create (module Constraint)

  let length = Hash_set.length

  let pp formatter subs = Format.fprintf formatter "%a" (Format.pp_print_list ~pp_sep:(string_sep ", ") Constraint.pp) (Hash_set.to_list subs)

  let map ~f t =
    let new_t = Hash_set.create (module Constraint) in
    Hash_set.iter ~f:(fun a -> Hash_set.add new_t (f a)) t;
    new_t

  let first t = match Hash_set.to_list t
    with
    | hd :: _ -> Some hd
    | [] -> None

  let remove t e = Hash_set.strict_remove_exn t e

  let find t = Hash_set.find t

  let rec add constraints c =
    (* Format.printf "adding %a\n" Constraint.pp c; *)
    match c with
    (* ignore trivial constraints *)
    | (x, y) when Type.equal x y -> ()
    | (Type.Number n1 , Type.Number n2) when Type.is_bounded_by n1 n2 -> ()
    (* keep nontrivial constraints *)
    | (Set x, Set y) -> (
      add constraints (x, y)
    )
    | (Sequence x, Sequence y) -> (
      add constraints (x, y)
    )
    | _ -> (
      Hash_set.add constraints c
    )
    (* | _ -> raise (TypeError "Type error") *)
  
end

(* maps type variables to types *)
(* the goal of type checking is to come up with substitutions to satisfy every constraint *)
module Substitution = struct
  (* {t / 'x}, e.g. substitute 'x with type t *)
  (* sub (x, y) should replace all instances of y (usually a type variable) with x *)
  type t = Type.t * Type.t
  [@@deriving eq, show, sexp, hash, ord]

  let pp formatter sub = Format.fprintf formatter "{ %a / %a }" Type.pp (fst sub) Type.pp (snd sub)
end

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
let apply_c subs (a, b) = (apply subs a, apply subs b)

(* apply substitution to multiple constraints at once *)
let apply_cs subs cs = Constraints.map ~f:(apply_c subs) cs

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

(* apply arithmetic promotion when possible *)
(* this is also where invalid operations are detected (such as adding a number and a set) *)
let simplify ~final t =
  (* Format.printf "Simplifing %a (%a)\n" Type.pp t Format.pp_print_bool final; *)
  let rec recurse t = match t with
  | Type.Bound (a, Plus, b) -> (
    match (recurse a, recurse b) with
    | (Type.Number a, Number b) -> Number (Type.widen a b)
    | _ when not final -> t
    | _ -> raise (TypeError (Format.asprintf "Could not infer numeric type for %a + %a" Type.pp a Type.pp b))
  )
  | Type.Bound (a, Times, b) -> (
    match (recurse a, recurse b) with
    | (Type.Number a, Number b) -> Number (Type.widen a b)
    | _ when not final -> t
    | _ -> raise (TypeError (Format.asprintf "Could not infer numeric type for %a * %a" Type.pp a Type.pp b))
  )
  | Type.Bound (a, Minus, b) -> (
    match (recurse a, recurse b) with
      (* a - b could be negative unless we prove a > b *)
    | (Type.Number Natural, Number Natural) -> Number Integer
    | (Type.Number a, Number b) -> Number (Type.widen a b)
    | _ when not final -> t
    | _ -> raise (TypeError (Format.asprintf "Could not infer numeric type for %a - %a" Type.pp a Type.pp b))
  )
  | Type.Bound (a, Frac, b) -> (
    match (recurse a, recurse b) with
    | (Type.Number Natural, Number Natural)
    | (Type.Number Natural, Number Integer)
    | (Type.Number Integer, Number Natural)
    | (Type.Number Integer, Number Integer) -> Number Rational
    | (Type.Number a, Number b) -> Number (Type.widen a b)
    | _ when not final -> t
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
    | (Type.Number a, Number b) -> Number (Type.widen a b)
    | _ when not final -> t
    | _ -> raise (TypeError (Format.asprintf "Could not infer numeric type for %a ^ %a" Type.pp a Type.pp b))
  )
  | Set u -> Set (recurse u)
  | Sequence u -> Sequence (recurse u)
  | Any _ -> (
      (* if performing final simplification, then assume variables without types to be reals *)
      if final then Number Real else t
    )
  | _ -> t
  in
  recurse t

(* TODO: this algorithm is not always correct... fix later ;_; *)
let unify constraints =
  let rec recurse (cs: Constraints.t) (acc: Substitutions.t) =
    if (Constraints.length cs = 0) then
      acc
    else
      (* let _ = Format.printf "Constraints: %a\n" Constraints.pp cs in *)
      (* pick out any element from the set of constraints *)
      let hd = Option.value_exn (Constraints.first cs) in
      (* Format.printf "hd: %a\n" Constraint.pp hd; *)
      Constraints.remove cs hd;
      let () = match Constraints.find cs ~f:(fun x -> Constraint.equal x hd) with
        | Some _ -> Format.printf "wtf\n"
        | None -> ()
      in
      match hd with
      (* basic cases that don't require any logic *)
      (* | Equal (Type.Number n1 , Type.Number n2) when Type.equal_number n1 n2 -> recurse cs acc *)
      | (x, y) when Type.equal x y -> recurse cs acc

      | (Set x, Set y) -> (
        Constraints.add cs (x, y);
        recurse cs acc
      )
      | (Tuple x, y) -> (
        Constraints.add cs (x, y);
        recurse cs acc
      )
      | (x, Tuple y) -> (
        Constraints.add cs (x, y);
        recurse cs acc
      )
      | (Sequence x, Sequence y) -> (
        Constraints.add cs (x, y);
        recurse cs acc
      )
      (* arithmetic and type promotion *)
      | (Type.Number n1 , Type.Number n2) when Type.is_bounded_by n1 n2 -> recurse cs acc

      (* do we need to check upper/lower bounds? *)
      | (Any t, u) -> (
        if not (occurs_in (Any t) u) then
          let s = (simplify ~final:false u, Type.Any t) in
          recurse (apply_cs [s] cs) (s :: acc)
        else
          (* raise (TypeError "Could not unify types 1") *)
          recurse cs acc
      )
      | (u, Any t) -> (
        (* Format.printf "Replacing %a with %a (<)\n" Var.pp t Type.pp u; *)
        (* let hi = Constraints.get_bounds upper t in *)
        (* Format.printf "Current upper bounds: [%a]\n" (Format.pp_print_list ~pp_sep:(string_sep ", ") Type.pp) hi; *)
        (* let lo = Constraints.get_bounds lower t in *)
        (* Format.printf "Current lower bounds: [%a]\n" (Format.pp_print_list ~pp_sep:(string_sep ", ") Type.pp) lo; *)
        if not (occurs_in (Any t) u) then
          let s = (simplify ~final:false u, Type.Any t) in
          recurse (apply_cs [s] cs) (s :: acc)
        else
          (* raise (TypeError "Could not unify types 2") *)
          recurse cs acc
      )
      (* ignore all other cases *)
      | _ -> recurse cs acc
      (* | _ -> raise (TypeError (Format.asprintf "Could not unify types 5: %a with constraints %a" Constraint.pp hd Constraints.pp cs)) *)
  in
  let copy = Constraints.map ~f:id constraints in (* prevent original set from being modified *)
  (* need to reverse list since it was built up in reverse in the above recursion *)
  List.rev (recurse copy [])
