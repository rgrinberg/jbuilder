open Stdune

type t

module Alias : sig
  type t =
    { alias : Alias.t
    ; rec_ : bool
    }
end

val alias : Alias.t -> t
val file : Path.t -> t
val env : Env.Var.t -> t
val universe : t

val compare : t -> t -> Ordering.t

val pp : t Fmt.t

module Set : sig
  include Set.S with type elt = t

  val has_universe : t -> bool

  val paths : t -> stamps:(Alias.t -> Path.Set.t) -> Path.Set.t

  val encode : t -> Dune_lang.t

  val trace
    : t
    -> env:Env.t
    -> stamps:(Alias.t -> Path.Set.t)
    -> (string * Digest.t) list

  val add_paths : t -> Path.Set.t -> t

  val parallel_iter
    :  t
    -> f:(Path.t -> unit Fiber.t)
    -> stamps:(Alias.t -> Path.Set.t)
    -> unit Fiber.t

  val pp : t Fmt.t

  val dirs : t -> Path.Set.t
end
