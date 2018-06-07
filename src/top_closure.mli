open Stdune

module type Keys = sig
  type t
  type elt
  val empty : t
  val add : t -> elt -> t
  val mem : t -> elt -> bool
end

module type Keys_mutable = sig
  type t
  type elt
  val create : unit -> t
  val add : t -> elt -> unit
  val mem : t -> elt -> bool
end

module Of_table (T : Hashtbl.S) : Keys_mutable
  with type t = unit T.t and type elt = T.key

module To_mutable(K : Keys) : Keys_mutable with type elt = K.elt

(* module Of_table(T : Hashtbl.S) : Keys_mutable
 *   with type t = unit T.t and type elt = T.key *)

module type S = sig
  type key

  (** Returns [Error cycle] in case the graph is not a DAG *)
  val top_closure
    :  key:('a -> key)
    -> deps:('a -> 'a list)
    -> 'a list
    -> ('a list, 'a list) result
end

module Int    : S with type key := int
module String : S with type key := string

module Make(Keys : Keys_mutable) : S with type key := Keys.elt
