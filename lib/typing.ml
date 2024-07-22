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
      | Top
      | Bottom
    [@@deriving eq, sexp, ord, compare]
  end = struct
    type t =
      | TC of string * Variance.t list * Comparable.Set.t
        (* name, dependent type variances, super type(-constructor)s *)
      | Top
      | Bottom
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

  let get_name (tc : t) : string =
    match tc with TC (name, _, _) -> name | Top -> "Top" | Bottom -> "Bottom"

  let get_variances (tc : t) : Variance.t list =
    match tc with TC (_, variances, _) -> variances | Top -> [] | Bottom -> []

  let get_direct_supers (tc : t) : Set.t =
    match tc with
    | TC (_, _, direct_supers) -> direct_supers
    | Top -> Set.empty
    | Bottom -> Set.empty

  let rec get_supers (tc : t) : Set.t =
    Core.Set.union (get_direct_supers tc) (get_supers =<<% get_direct_supers tc)

  let is_super (super : t) (sub : t) : bool =
    match (super, sub) with
    | Top, _ -> true
    | _, Bottom -> true
    | _ -> Core.Set.mem (get_supers sub) super

  let is_sub (sub : t) (super : t) : bool = is_super super sub

  module BuiltIn = struct
    let function_c : t =
      TC ("Function", [ Contravariant; Covariant ], Set.of_list [])

    let complex_c : t = TC ("Complex", [], Set.of_list [])
    let real_c : t = TC ("Real", [], Set.of_list [])
    let rational_c : t = TC ("Rational", [], Set.of_list [ real_c ])
    let int_c : t = TC ("Int", [], Set.of_list [ rational_c ])
    let nat_c : t = TC ("Nat", [], Set.of_list [ int_c ])
    let prime_c : t = TC ("Prime", [], Set.of_list [ nat_c ])
    let pair_c : t = TC ("Pair", [ Covariant; Covariant ], Set.of_list [])
    let list_c : t = TC ("List", [ Covariant ], Set.of_list [])
    let set_c : t = TC ("Set", [ Covariant ], Set.of_list [])
    let bin_tree_c : t = TC ("BinaryTree", [ Covariant ], Set.of_list [])
    let graph_c : t = TC ("Graph", [ Covariant; Covariant ], Set.of_list [])
  end
end

module rec Type : sig
  module FTypeIdentifier : sig
    type t

    val of_int : int -> t
  end

  type t =
    | Concrete of ConcreteType.t
    | FType of FTypeIdentifier.t
    | Union of t * t
  [@@deriving eq, sexp, ord, compare]

  val is_super : t -> t -> bool
  val is_sub : t -> t -> bool
end = struct
  module FTypeIdentifier = Int

  type t =
    | Concrete of ConcreteType.t
    | FType of FTypeIdentifier.t
    | Union of t * t
  [@@deriving eq, sexp, ord, compare]

  let rec resolve_union ty =
    match ty with
    | Union (x, y) -> resolve_union x @ resolve_union y
    | _ -> [ ty ]

  let rec is_super super sub =
    match (super, sub) with
    | Concrete super_c, Concrete sub_c -> ConcreteType.is_super super_c sub_c
    | FType super_id, FType sub_id -> FTypeIdentifier.equal super_id sub_id
    | FType _, Concrete ct when ConcreteType.is_bottom ct -> true
    | Concrete ct, FType _ when ConcreteType.is_top ct -> true
    | Union _, _ | _, Union _ ->
        let super_resolve = resolve_union super in
        let sub_resolve = resolve_union sub in
        List.for_all sub_resolve ~f:(fun sub_t ->
            List.exists super_resolve ~f:(is_sub sub_t))
    | _ -> false

  and is_sub sub super = is_super super sub

  module BuiltIn = struct end
end

and ConcreteType : sig
  type t [@@deriving eq, sexp, ord, compare]

  val constr : TypeConstructor.t -> Type.t list -> t
  val get_type_constructor : t -> TypeConstructor.t
  val get_type_vars : t -> (Type.t * Variance.t) list
  val is_super : t -> t -> bool
  val is_sub : t -> t -> bool
  val is_top : t -> bool
  val is_bottom : t -> bool
end = struct
  type t = TypeConstructor.t * Type.t list [@@deriving eq, sexp, ord, compare]

  exception BadTypeInstantiation of string

  let constr tc type_vars =
    let expected_type_vars = List.length (TypeConstructor.get_variances tc) in
    let num_type_vars = List.length type_vars in
    if expected_type_vars = num_type_vars then (tc, type_vars)
    else
      raise
        (BadTypeInstantiation
           ("Type constructor \""
           ^ TypeConstructor.get_name tc
           ^ "\" expected "
           ^ string_of_int expected_type_vars
           ^ " type-vars, but got "
           ^ string_of_int num_type_vars))

  let get_type_constructor ct = match ct with tc, _ -> tc

  let get_type_vars ct =
    match ct with
    | tc, type_vars -> List.zip_exn type_vars (TypeConstructor.get_variances tc)

  let rec is_super super sub =
    TypeConstructor.is_super
      (get_type_constructor super)
      (get_type_constructor sub)
    && List.for_all
         (List.zip_shorter (get_type_vars super) (get_type_vars sub))
         ~f:(fun ((super_type_arg, super_var), (sub_type_arg, sub_var)) ->
           Type.equal super_type_arg sub_type_arg
           || Variance.equal
                (Variance.most_covariant super_var sub_var)
                Variance.Covariant
              && Type.is_super super_type_arg sub_type_arg
           || Variance.equal
                (Variance.most_contravariant super_var sub_var)
                Variance.Covariant
              && Type.is_sub super_type_arg sub_type_arg)

  and is_sub sub super = is_super super sub

  let is_top ct = match ct with TypeConstructor.T.Top, _ -> true | _ -> false

  let is_bottom ct =
    match ct with TypeConstructor.T.Bottom, _ -> true | _ -> false
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
