include Core
include Util

module ComparableExtension (C : Core.Comparable.S) = struct
  include C

  module Set = struct
    include C.Set
    include Comparable.Make (C.Set)

    let flatten (s : Set.t) : t = C.Set.union_list (Core.Set.to_list s)

    let bind ~(f : C.t -> t) (x : t) : C.Set.t =
      C.Set.union_list (f |<<: Core.Set.to_list x)

    module Infix = struct
      let ( >>=% ) (x : t) (f : C.t -> t) = bind x ~f
      let ( =<<% ) (f : C.t -> t) (x : t) = bind x ~f
      let ( >>|% ) (x : t) (f : C.t -> C.t) = map x ~f
      let ( |<<% ) (f : C.t -> C.t) (x : t) = map x ~f
      let ( <-<% ) (f : C.t -> unit) (x : t) = List.iter ~f (Core.Set.to_list x)
      let ( >->% ) (x : t) (f : C.t -> unit) = List.iter ~f (Core.Set.to_list x)
    end
  end
end
