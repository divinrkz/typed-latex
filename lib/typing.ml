open Core
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

(* the name of a variable that needs to be type checked *)
type identifier = string

type type_var = {
  id: int
}
[@@deriving eq, show]

let pp_type_var formatter var = Format.fprintf formatter "'t%i" var.id

(* instantiates a new, unique type variable each call *)
let fresh = 
  let counter = ref 0 in
  fun () ->
    let t = { id = !counter } in
    counter := !counter + 1;
    t

type math_type =
  | Number
  | Bool
  | Set of math_type
  | Function of math_type list * math_type
  | Any of type_var

let string_sep str formatter = fun () -> Format.pp_print_string formatter str

let rec pp_math_type formatter math = match math with
  | Number -> Format.fprintf formatter "NUMBER"
  | Bool -> Format.fprintf formatter "BOOL"
  | Set t -> Format.fprintf formatter "SET<%a>" pp_math_type t
  | Function (u, v) -> Format.fprintf formatter  "[%a] -> %a" (Format.pp_print_list ~pp_sep:(string_sep ", ") pp_math_type) u pp_math_type v
  | Any t -> pp_type_var formatter t

(* mapping from type variables to types *)
(* type environment = identifier -> math_type *)

(* an assertion that one type is equivalent to another, i.e. t1 = t2 *)
type type_constraint = math_type * math_type
type type_constraints = type_constraint list

(* maps type variables to types *)
(* the goal of type checking is to come up with enough substitutions to satisfy every constraint *)
type substitution = math_type * type_var (* {t / 'x}, e.g. substitute 'x with type t *)
type substitutions = substitution list

let pp_substitution formatter sub = Format.fprintf formatter "{ %a / %a }" pp_math_type (fst sub) pp_type_var (snd sub)
let pp_substitutions formatter subs = Format.fprintf formatter "%a" (Format.pp_print_list ~pp_sep:(string_sep ", ") pp_substitution) subs

(* applies a type substitution to a type *)
let rec apply1 (sub: substitution) (t: math_type) =
  match t with
    | Number -> Number
    | Bool -> Bool
    | Set u -> Set (apply1 sub u)
    | Function (u, v) -> Function (List.map ~f:(apply1 sub) u, apply1 sub v)
    | Any u -> if equal_type_var (snd sub) u then (fst sub) else Any u

(* t (S1; S2) = (t S1) S2 *)
let rec apply (subs: substitutions) (t: math_type) =
  match subs with
    | [] -> t
    | hd :: tl -> apply tl (apply1 hd t)

(* (t1 = t2) S => t1 S = t2 S *)
let apply_c (subs: substitutions) (c: type_constraint) = (apply subs (fst c), apply subs (snd c))
let apply_cs (subs: substitutions) (cs: type_constraints) = List.map ~f:(apply_c subs) cs

let exists = function
  | Some _ -> true
  | None -> false

let rec occurs_in (t: type_var) (u: math_type) =
  match u with
    | Number -> false
    | Bool -> false
    | Set v -> occurs_in t v
    | Function (u, v) -> occurs_in t v || exists (List.find ~f:(occurs_in t) u)
    | Any v -> equal_type_var t v

exception TypeError of string

let rec unify constraints =
  match constraints with
    | [] -> []
    | hd :: tl -> (
        match hd with
          (* basic cases that don't require any logic *)
          | (Number, Number) -> unify tl
          | (Bool, Bool) -> unify tl
          | (Any t, Any u) when equal_type_var t u -> unify tl
          (* basic substitutions *)
          | (Any t, u) -> (
            if not (occurs_in t u) then
              let s = (u, t) in
              s :: unify (apply_cs [s] tl)
            else
              raise (TypeError "Could not unify types")
          )
          | (u, Any t) -> (
            if not (occurs_in t u) then
              let s = (u, t) in
              s :: unify (apply_cs [s] tl)
            else
              raise (TypeError "Could not unify types")
          )
          | (Function (x1, y1), Function (x2, y2)) -> (
            if (not (Int.equal (List.length x1) (List.length x2))) then
              raise (TypeError "asdf")
            else
              let new_constraints = (y1, y2) :: List.zip_exn x1 x2 in
              unify new_constraints
          )
          | _ -> raise (TypeError "Could not unify types")
    )

let test () =
  let t = fresh () in
  let x = Number in
  let y = Any t in
  let constraints = [(x, y)] in
  let subs = unify constraints in
  Format.printf "%a\n" pp_substitutions subs;
