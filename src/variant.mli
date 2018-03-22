(** Library variants *)

(** Library variants allow to select the implementation of a library
    at link time.

    They are directly mapped to findlib predicates.
*)

include Interned.S

(** Well-known variants *)
val ppx_driver : t
val mt         : t
val mt_posix   : t
val byte       : t
val native     : t
val plugin     : t

module Rules : sig
  type variant = t
  type 'a t

  val get : 'a t -> variants:Set.t -> 'a

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val make : default:'a -> (variant * 'a) list -> 'a t
  val of_meta_rules : Meta.Simplified.Rules.t -> string list t
end with type variant := t
