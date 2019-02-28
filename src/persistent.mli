open Stdune

module type Description = sig
  type t
  val name : string
  val version : int
end

(** Persistent value stored on disk *)
module Make(D : Description) : sig
  val to_out_string : D.t -> string
  val dump : Path.t -> D.t -> unit
  val load : Path.t -> D.t option
end
