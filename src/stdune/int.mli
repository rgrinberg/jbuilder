type t = int
val compare : t -> t -> Ordering.t
val hash : t -> int
val equal : t -> t -> bool

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
module Table : Hashtbl.S with type key = int
