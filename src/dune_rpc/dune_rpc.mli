(** Dune's rpc mechanism. Independent of any particular server implementation.

    The implementation is loosely modelled on jsonrpc. It defines the following
    concepts:

    Session - An active rpc session

    Request - A unique id with a payload sent by a client. A server must respond
    to every request

    Notification - A payload send by a client. A server must not respond to a
    notification

    It contains hooks that make it possible to use with any custom scheduler
    that uses fibers *)

open Stdune

module Id : sig
  (* Every rpc request must contain a unique identifier *)
  type t
end

module Payload : sig
  (* The payload for a request *)
  type t =
    { method_ : string
    ; params : Sexp.t
    }
end

module Request : sig
  type t = Id.t * Payload.t
end

module Session : sig
  (** Represents an active RPC session *)
  type t

  val notification : t -> Payload.t -> unit Fiber.t

  val request : t -> Request.t -> Sexp.t Fiber.t
end

module Handler : sig
  (** A handler for rpc requests and notifications. *)
  type t

  val create :
       on_request:(Session.t -> Request.t -> Sexp.t Fiber.t)
    -> on_notification:(Session.t -> Payload.t -> unit Fiber.t)
    -> on_init:(Session.t -> unit Fiber.t)
    -> t
end

val default_socket : unit -> Path.t

(** Where to connect if there's an active server running *)
val where : unit -> Path.t option

val add_rpc_to_env : Env.t -> Path.t -> Env.t

(** Functor to create a server implementation

    TODO: client implementation *)
module Make (S : sig
  type t

  val write : t -> Sexp.t -> unit Fiber.t

  val read : t -> Sexp.t option Fiber.t

  val close : t -> unit Fiber.t
end) : sig
  val run : S.t Fiber.Sequence.t -> Handler.t -> unit Fiber.t
end
