(** Threaded RPC Client & Server for Csexp payloads. This RPC implementation is
    entirely structure free and it is up to users of this library to impose some
    sort of conventions on the protocol.

    On Unix/Mac, RPC uses Unix docmain sockets. On Windows, named pipes are
    used.

    All functions reurning a fiber are non-blocking and run in their own thread. *)
open Stdune

module Scheduler : sig
  (** Hook into the fiber scheduler. *)
  type t =
    { on_event : Fiber.fill -> unit  (** Called when any rpc event completes *)
    ; register_pending_ivar : unit -> unit
          (** Called before any ivar is created *)
    ; thread : 'a 'b. ('a -> 'b) -> 'a -> Thread.t
          (** We spawn threads through this function in case the scheduler wants
              to block signals *)
    }
end

module Session : sig
  (** Rpc session backed by two threads. One thread for reading, and another for
      writing *)
  type t

  val create : in_channel -> out_channel -> Scheduler.t -> t

  val write : t -> Sexp.t -> unit Fiber.t

  (** If [read] returns [None], the session is closed and all subsequent reads
      will return [None]*)
  val read : t -> Sexp.t option Fiber.t

  val close : t -> unit Fiber.t
end

module Client : sig
  (** RPC Client *)
  type t

  val create : Path.t -> Scheduler.t -> t

  val stop : t -> unit

  val connect : t -> Session.t Fiber.t
end

module Server : sig
  (** RPC Server *)
  type t

  val create : Path.t -> backlog:int -> Scheduler.t -> t

  val stop : t -> unit

  val serve : t -> Session.t Fiber.Sequence.t Fiber.t
end
