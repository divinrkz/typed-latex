(* open Typing
open Util

module Construction : sig
  type t [@@deriving eq, sexp, ord, compare]
end = struct
  type t = Type.t * Symbol.Set.t [@@deriving eq, sexp, ord, compare]

  let constr ty symbols =
    (ty,
    symbols >>|: (function
    | 
    ))
    
end *)
