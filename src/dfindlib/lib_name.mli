open Stdune

type t = private string

val compare : t -> t -> Ordering.t

val of_string_exn : loc:Loc.t option -> string -> t

val to_string : t -> string

module Map : Map.S with type key = t
module Set : sig
  include Set.S with type elt = t
  val to_string_list : t -> string list
end

val root_lib : t -> t

val pp_quoted : t Fmt.t

val pp : t Fmt.t

val nest : t -> t -> t
