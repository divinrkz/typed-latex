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
  type t =
    | Number
    | Bool
    | Set of t
    | Any of Var.t
  [@@deriving eq, sexp, hash]

  let rec pp formatter math = match math with
    | Number -> Format.fprintf formatter "NUMBER"
    | Bool -> Format.fprintf formatter "BOOL"
    | Set t -> Format.fprintf formatter "SET<%a>" pp t
    | Any t -> Var.pp formatter t

end

module Constraint = struct
  type t =
    | Equal of Type.t * Type.t (* a declaration that two types are equivalent, e.g. t1 = t2 *)
  [@@deriving eq, sexp, hash]

  let compare a b = if equal a b then 0 else 1

  let pp formatter c = match c with
    | Equal (a, b) -> Format.fprintf formatter "[ %a = %a ]" Type.pp a Type.pp b
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
end

(* maps type variables to types *)
(* the goal of type checking is to come up with substitutions to satisfy every constraint *)
module Substitution = struct
  type t = Type.t * Var.t (* {t / 'x}, e.g. substitute 'x with type t *)
  [@@deriving eq, sexp, hash]

  let compare a b = if equal a b then 0 else 1

  let pp formatter sub = Format.fprintf formatter "{ %a / %a }" Type.pp (fst sub) Var.pp (snd sub)
end

module SubstitutionList = Hash_set.Make (Substitution)

module Substitutions = struct
  type t = Substitution.t list

  let pp formatter subs = Format.fprintf formatter "%a" (Format.pp_print_list ~pp_sep:(string_sep ", ") Substitution.pp) subs
end

(* applies a type substitution to a type *)
let rec apply1 (sub: Substitution.t) (t: Type.t) =
  match t with
    | Number -> Type.Number
    | Bool -> Type.Bool
    | Set u -> Type.Set (apply1 sub u)
    | Any u -> if Var.equal (snd sub) u then (fst sub) else Type.Any u

(* t (S1; S2) = (t S1) S2 *)
let rec apply (subs: Substitutions.t) (t: Type.t) =
  match subs with
    | [] -> t
    | hd :: tl -> apply tl (apply1 hd t)

(* (t1 = t2) S => t1 S = t2 S *)
let apply_c (subs: Substitutions.t) (c: Constraint.t) =
  match c with
  | Equal (a, b) -> Constraint.Equal (apply subs a, apply subs b)

let apply_cs (subs: Substitutions.t) (cs: Constraints.t) = Constraints.map ~f:(apply_c subs) cs

let exists = function
  | Some _ -> true
  | None -> false

let rec occurs_in t (u: Type.t) =
  match u with
    | Number -> false
    | Bool -> false
    | Set v -> occurs_in t v
    | Any v -> Var.equal t v

exception TypeError of string

let unify constraints =
  let rec recurse (cs: Constraints.t) (acc: Substitutions.t) =
    if (Hash_set.length cs = 0) then
      acc
    else
      (* pick out any element from the set of constraints *)
      let hd = Option.value_exn (Constraints.first cs) in
      Hash_set.remove cs hd;
      match hd with
        (* basic cases that don't require any logic *)
        | Equal (Type.Number, Type.Number) -> recurse cs acc
        | Equal (Bool, Bool) -> recurse cs acc
        | Equal (Any t, Any u) when Var.equal t u -> recurse cs acc
        (* basic substitutions *)
        | Equal (Any t, u) -> (
          if not (occurs_in t u) then
            let s = (u, t) in
            recurse (apply_cs [s] cs) (s :: acc)
          else
            raise (TypeError "Could not unify types 1")
        )
        | Equal (u, Any t) -> (
          if not (occurs_in t u) then
            let s = (u, t) in
            recurse (apply_cs [s] cs) (s :: acc)
          else
            raise (TypeError "Could not unify types 2")
        )
        | Equal (Set x, Set y) -> (
          Hash_set.add cs (Equal (x, y));
          recurse cs acc
        )
        | _ -> raise (TypeError "Could not unify types 3")
  in
  let copy = Hash_set.copy constraints in (* prevent original set from being modified *)
  recurse copy []

let test () =
  let t0 = Var.fresh () in
  let x = Type.Any t0 in
  let y = Type.Number in
  let constraints = Constraints.of_list [Equal (x, y)] in
  let subs = unify constraints in
  Format.printf "%a\n" Substitutions.pp subs;
  let p = apply subs x in
  Format.printf "%a\n" Type.pp p;
  ()

