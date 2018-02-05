(** Fibers *)

open Import

(** {1 Generals} *)

(** Type of fiber. A fiber represent a suspended computation. Note that using the same
    fiber twice will execute it twice, which is probably not what you want. To share the
    result of a fiber, use an [Ivar.t].  *)
type 'a t

(** Create a fiber that has already terminated. *)
val return : 'a -> 'a t

(** Fiber that never completes. *)
val never : 'a t

module O : sig
  (** [>>>] is a sequencing operator. [a >>> b] is the fiber that
      first executes [a] and then [b]. *)
  val (>>>) : unit t -> 'a t -> 'a t

  (** [>>=] is similar to [>>>] except that the result of the first
      fiber is used to create the second one. *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  (** [t >>| f] is the same as [t >>= fun x -> return (f x)] but
      slightly more efficient. *)
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
end

(** {1 Combining} *)

(** The following functions allow to combine two or more fibers. Note that when the
    execution of a fiber fails because of an exception, the other fibers will continue to
    run.

    This is why combining functions always take [unit -> _ t] functions rather than [_ t]
    values directly.
*)

val fork_and_join : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t

(** [fork_and_join_unit f g] is the same as [fork_and_join f g >>| snd] but slightly more
    efficient. *)
val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

val nfork_and_join : 'a list -> f:('a -> 'b t) -> 'b list t

(** Same as [nfork_and_join l ~f >>| ignore] bit more efficient. *)
val nfork_and_join_unit : 'a list -> f:('a -> unit t) -> unit t

(** {1 Local storage} *)

(** Variables local to a fiber *)
module Var : sig
  type 'a fiber = 'a t
  type 'a t

  (** Create a new variable *)
  val create : unit -> 'a t

  (** [get var] is a fiber that reads the value of [var] *)
  val get : 'a t -> 'a option fiber

  (** Same as [get] but raises if [var] is unset. *)
  val get_exn : 'a t -> 'a fiber

  (** [set var value fiber] sets [var] to [value] during the execution
      of [fiber].

      For instance, the following fiber always evaluate to [true]:

      {[
        set v x (get_exn v >>| fun y -> x = y)
      ]}
 *)
  val set : 'a t -> 'a -> 'b fiber -> 'b fiber
end with type 'a fiber := 'a t

(** {1 Error handling} *)

(** [with_error_handler f ~on_error] calls [on_error] for every exception raised during
    the execution of [f]. This include exceptions raised when calling [f ()] or during the
    execution of fibers after [f ()] has returned. Exceptions raised by [on_error] are
    passed on to the parent error handler.

    It is guaranteed that after the fiber has returned a value, [on_error] will never be
    called.
*)
val with_error_handler
  :  (unit -> 'a t)
  -> on_error:(exn -> unit)
  -> 'a t

(** If [t] completes without raising, then [wait_errors t] is the same as [t () >>| fun x
    -> Ok x]. However, if the execution of [t] is aborted by an exception, then
    [wait_errors t] will complete and yield [Error ()].

    Note that [wait_errors] only completes after all sub-fibers have completed. For
    instance, in the following code [wait_errors] will only complete after 3s:

    {[
      wait_errors
        (fork_and_join
           (fun () -> sleep 1 >>| fun () -> raise Exit)
           (fun () -> sleep 3))
    ]}
*)
val wait_errors : 'a t -> ('a, unit) result t

(** [fold_errors f ~init ~on_error] calls [on_error] for every exception raised during the
    execution of [f]. This include exceptions raised when calling [f ()] or during the
    execution of fibers after [f ()] has returned.

    Exceptions raised by [on_error] are passed on to the parent error handler. *)
val fold_errors
  :  (unit -> 'a t)
  -> init:'b
  -> on_error:(exn -> 'b -> 'b)
  -> ('a, 'b) result t

(** [catch_errors f] is:

    {[
      fold_errors f
        ~init:[]
        ~on_error:(fun e l -> e :: l)
    ]}
*)
val catch_errors
  :  (unit -> 'a t)
  -> ('a, exn list) result t

(** [finalize f ~finally] runs [finally] after [f ()] has terminated,
    whether it fails or succeeds. *)
val finalize
  :  (unit -> 'a t)
  -> finally:(unit -> unit t)
  -> 'a t

(** {1 Synchronization} *)

(** Write once variables *)
module Ivar : sig
  type 'a fiber = 'a t

  (** A ivar is a synchronization variable that can be written only
      once. *)
  type 'a t

  (** Create a new empty ivar. *)
  val create : unit -> 'a t

  (** Read the contents of the ivar. *)
  val read : 'a t -> 'a fiber

  (** Fill the ivar with the following value. This can only be called
      once for a given ivar. *)
  val fill : 'a t -> 'a -> unit fiber
end with type 'a fiber := 'a t

module Mutex : sig
  type 'a fiber = 'a t
  type t
  val create : unit -> t
  val with_lock : t -> (unit -> 'a fiber) -> 'a fiber
end with type 'a fiber := 'a t

(** {1 Running fibers} *)

module Scheduler : sig
  (** [go ?log t] runs the following fiber until it terminates. If it becomes clear that
      the fiber will never complete, for instance because of an uncaught exception,
      {!Never} is raised. *)
  val go : ?log:Log.t -> 'a t -> 'a

  exception Never

  (** Wait for the following process to terminate *)
  val wait_for_process : int -> Unix.process_status t

  (** Scheduler informations *)
  type info =
    { log : Log.t
    (** Logger *)
    ; original_cwd : string
    (** Working directory at the time [go] was called *)
    }

  (** Wait until less tham [!Clflags.concurrency] external processes
      are running and return the scheduler informations. *)
  val wait_for_available_job : info t
end
