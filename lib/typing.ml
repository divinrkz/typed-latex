open Comparable_extension

module TypeConstructor = struct
  type variance = Invariant | Covariant | Contravariant
  [@@deriving eq, show, sexp, hash, ord, compare]

  module rec T : sig
    type t =
      | TC of string * variance list * Comparable.Set.t
        (* name, dependent type variances, super type(-constructor)s *)
    [@@deriving eq, sexp, ord, compare]
  end = struct
    type t =
      | TC of string * variance list * Comparable.Set.t
        (* name, dependent type variances, super type(-constructor)s *)
    [@@deriving eq, sexp, ord, compare]
  end

  and Comparable : (Core.Comparable.S with type t := T.t) =
    Core.Comparable.Make (T)

  module TypedComparable = struct
    type t = T.t

    include Comparable
  end

  module ExtendedComparable = ComparableExtension (TypedComparable)
  include T
  include ExtendedComparable
  include Set.Infix

  let get_name (tc : t) : string = match tc with TC (name, _, _) -> name

  let get_variances (tc : t) : variance list =
    match tc with TC (_, variances, _) -> variances

  let get_direct_supers (tc : t) : Set.t =
    match tc with TC (_, _, direct_supers) -> direct_supers

  let rec get_supers (tc : t) : Set.t =
    Core.Set.union (get_direct_supers tc) (get_supers =<<% get_direct_supers tc)
end

module TypeInstance : sig
  type t [@@deriving eq, sexp, ord, compare]

  val constr : TypeConstructor.t -> t list -> t
  (* val get_type_constructor : t -> TypeConstructor.t
     val get_type_vars : t -> (t * TypeConstructor.variance) list *)
end = struct
  type t = TypeT of TypeConstructor.t * t list
  [@@deriving eq, sexp, ord, compare]

  exception BadTypeInstantiation of string

  let constr tc type_vars =
    let expected_type_vars = List.length (TypeConstructor.get_variances tc) in
    let num_type_vars = List.length type_vars in
    if expected_type_vars = num_type_vars then TypeT (tc, type_vars)
    else
      raise
        (BadTypeInstantiation
           ("Type constructor \""
           ^ TypeConstructor.get_name tc
           ^ "\" expected "
           ^ string_of_int expected_type_vars
           ^ " type-vars, but got "
           ^ string_of_int num_type_vars))
end

(* module Type = struct

   	type tc =
   	| TC of string * variance list * tc Set.t  (* name, dependent type variances, super type(-constructor)s *)  (* not actual syntax for set *)
   	type ti =
   	| TypeT of tc * t list  (* type constructor, dependent type values *)
   	type t =
   	| Union of ti Set.t  (* union types *)
   	| Unknown
   	type value
   	| Value of t  (* type *)
   	type symbol
   	| NamedSymbol of string * t  (* name, type *)

   	let top_c = TC "Top" [] {}  (* Not actually syntax for creating a set *)
   	let function_c = TC "Function" [ Contravariant; Covariant ] { top_c }
   	let complex_c = TC "Complex" [] { top_c }
   	let real_c = TC "Real" [] { complex_c }  (* No need to also list top_c: should be inferred from complex_c *)
   	let rational_c = TC "Rational" [] { real_c }
   	let int_c = TC "Int" [] { rational_c }
   	let nat_c = TC "Nat" [] { int_c }
   	let prime_c = TC "Prime" [] { nat_c }
   	let pair_c = TC "Pair" [ Covariant; Covariant ] { top_c }
   	let list_c = TC "List" [ Covariant ] { top_c }
   	let set_c = TC "Set" [ Covariant ] { top_c }
   	let bin_tree_c = TC "BinaryTree" [ Covariant ] { top_c }
   	let graph_c = TC "Graph" [ Covariant; Covariant ] { top_c }
   end *)
