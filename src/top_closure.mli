open Stdune

module type Keys = sig
  type t
  type elt
  val empty : t
  val add : t -> elt -> t
  val mem : t -> elt -> bool
end

module type Container = sig
  type t
  type elt
  val is_empty : t -> bool
  val iter : t -> f:(elt -> unit) -> unit
end

module type S = sig
  type key

  (** Returns [Error cycle] in case the graph is not a DAG *)
  val top_closure
    :  key:('a -> key)
    -> deps:('a -> 'a list)
    -> 'a list
    -> ('a list, 'a list) result
end

module type S0 = sig
  type key
  type deps
  type elt

  (** Returns [Error cycle] in case the graph is not a DAG *)
  val top_closure
    :  key:(elt -> key)
    -> deps:(elt -> deps)
    -> deps
    -> (elt list, elt list) result
end

module Int    : S with type key := int
module String : S with type key := string

module C0(C : Container)(Keys : Keys)
  : S0 with type key := Keys.elt
        and type deps := C.t
        and type elt := C.elt

module Make(Keys : Keys) : S with type key := Keys.elt
