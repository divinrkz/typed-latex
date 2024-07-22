open Comparable_extension

module Variance : sig
  type t = Covariant | Invariant | Contravariant
  [@@deriving eq, show, sexp, hash, ord, compare]

  include Comparable.S with type t := t

  val most_covariant : t -> t -> t
  val most_contravariant : t -> t -> t
end = struct
  module T = struct
    type t = Covariant | Invariant | Contravariant
    [@@deriving eq, show, sexp, hash, ord, compare]
  end

  include T
  include Comparable.Make (T)

  let most_covariant = min
  let most_contravariant = max
end

module TypeConstructor = struct
  module rec T : sig
    type t =
      | TC of string * Variance.t list * Comparable.Set.t
        (* name, dependent type variances, super type(-constructor)s *)
    [@@deriving eq, sexp, ord, compare]
  end = struct
    type t =
      | TC of string * Variance.t list * Comparable.Set.t
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

  let get_variances (tc : t) : Variance.t list =
    match tc with TC (_, variances, _) -> variances

  let get_direct_supers (tc : t) : Set.t =
    match tc with TC (_, _, direct_supers) -> direct_supers

  let rec get_supers (tc : t) : Set.t =
    Core.Set.union (get_direct_supers tc) (get_supers =<<% get_direct_supers tc)

  let is_super (super : t) (sub : t) : bool =
    Core.Set.mem (get_supers sub) super

  let is_sub (sub : t) (super : t) : bool = is_super super sub

  module BuiltIn = struct
    let top_c : t = TC ("Top", [], Set.of_list [])

    let function_c : t =
      TC ("Function", [ Contravariant; Covariant ], Set.of_list [ top_c ])

    let complex_c : t = TC ("Complex", [], Set.of_list [ top_c ])
    let real_c : t = TC ("Real", [], Set.of_list [ top_c ])
    let rational_c : t = TC ("Rational", [], Set.of_list [ real_c ])
    let int_c : t = TC ("Int", [], Set.of_list [ rational_c ])
    let nat_c : t = TC ("Nat", [], Set.of_list [ int_c ])
    let prime_c : t = TC ("Prime", [], Set.of_list [ nat_c ])
    let pair_c : t = TC ("Pair", [ Covariant; Covariant ], Set.of_list [ top_c ])
    let list_c : t = TC ("List", [ Covariant ], Set.of_list [ top_c ])
    let set_c : t = TC ("Set", [ Covariant ], Set.of_list [ top_c ])
    let bin_tree_c : t = TC ("BinaryTree", [ Covariant ], Set.of_list [ top_c ])

    let graph_c : t =
      TC ("Graph", [ Covariant; Covariant ], Set.of_list [ top_c ])
  end
end

module TypeInstance : sig
  type t [@@deriving eq, sexp, ord, compare]

  val constr : TypeConstructor.t -> t list -> t
  val get_type_constructor : t -> TypeConstructor.t
  val get_type_vars : t -> (t * Variance.t) list
  val is_super : t -> t -> bool
  val is_sub : t -> t -> bool
end = struct
  type t = TypeI of TypeConstructor.t * t list
  [@@deriving eq, sexp, ord, compare]

  exception BadTypeInstantiation of string

  let constr tc type_vars =
    let expected_type_vars = List.length (TypeConstructor.get_variances tc) in
    let num_type_vars = List.length type_vars in
    if expected_type_vars = num_type_vars then TypeI (tc, type_vars)
    else
      raise
        (BadTypeInstantiation
           ("Type constructor \""
           ^ TypeConstructor.get_name tc
           ^ "\" expected "
           ^ string_of_int expected_type_vars
           ^ " type-vars, but got "
           ^ string_of_int num_type_vars))

  let get_type_constructor ti = match ti with TypeI (tc, _) -> tc

  let get_type_vars ti =
    match ti with
    | TypeI (tc, type_vars) ->
        List.zip_exn type_vars (TypeConstructor.get_variances tc)

  let rec is_super super sub =
    TypeConstructor.is_super
      (get_type_constructor super)
      (get_type_constructor sub)
    && List.for_all
         (List.zip_shorter (get_type_vars super) (get_type_vars sub))
         ~f:(fun ((super_type_arg, super_var), (sub_type_arg, sub_var)) ->
           equal super_type_arg sub_type_arg
           || Variance.equal
                (Variance.most_covariant super_var sub_var)
                Variance.Covariant
              && is_super super_type_arg sub_type_arg
           || Variance.equal
                (Variance.most_contravariant super_var sub_var)
                Variance.Covariant
              && is_sub super_type_arg sub_type_arg)

  and is_sub sub super = is_super super sub

  module BuiltIn = struct end
end

module Type : sig
  module FTypeIdentifier : sig
    type t

    val of_int : int -> t
  end

  type t = Known of TypeInstance.t | FType of FTypeIdentifier.t * t * t
end = struct
  module FTypeIdentifier = Int

  type t = Known of TypeInstance.t | FType of FTypeIdentifier.t * t * t
end

module Value : sig
  type t = Value of Type.t
end = struct
  type t = Value of Type.t
end

module Symbol : sig
  type t = NamedSymbol of string * Type.t
end = struct
  type t = NamedSymbol of string * Type.t
end
