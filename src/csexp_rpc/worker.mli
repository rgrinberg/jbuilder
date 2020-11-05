(** Simple queue that is consumed by its own thread *)

type thread = { create : 'a 'b. ('a -> 'b) -> 'a -> Thread.t }

type 'work t

val create : thread -> ('a -> unit) -> 'a t

val add_work : 'a t -> 'a -> (unit, [ `Stopped ]) result

val stop : _ t -> unit
