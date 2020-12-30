open Stdune
open Fiber.O

module Scheduler = struct
  type t =
    { on_event : Fiber.fill -> unit
    ; register_pending_ivar : unit -> unit
    ; thread : 'a 'b. ('a -> 'b) -> 'a -> Thread.t
    }
end

module Async : sig
  type t

  val create : Scheduler.t -> t

  val task : t -> f:(unit -> 'a) -> 'a Or_exn.t Fiber.t

  val task_exn : t -> f:(unit -> 'a) -> 'a Fiber.t

  val stop : t -> unit
end = struct
  type task = Task : 'a Or_exn.t Fiber.Ivar.t * (unit -> 'a) -> task

  type t =
    { worker : task Worker.t
    ; scheduler : Scheduler.t
    }

  let stop t = Worker.stop t.worker

  let create (scheduler : Scheduler.t) =
    let do_ (Task (ivar, f)) =
      let res = Result.try_with f in
      scheduler.on_event (Fiber.Fill (ivar, res))
    in
    let worker = Worker.create { create = scheduler.thread } do_ in
    { worker; scheduler }

  let task (t : t) ~f =
    t.scheduler.register_pending_ivar ();
    let ivar = Fiber.Ivar.create () in
    match Worker.add_work t.worker (Task (ivar, f)) with
    | Ok () -> Fiber.Ivar.read ivar
    | Error `Stopped -> Code_error.raise "worker stopped" []

  let task_exn t ~f =
    let+ res = task t ~f in
    match res with
    | Ok s -> s
    | Error e -> raise e
end

module Session_id = Id.Make ()

module Session = struct
  module Id = Session_id

  type t =
    { out_channel : out_channel
    ; in_channel : in_channel
    ; id : Id.t
    ; writer : Async.t
    ; reader : Async.t
    ; scheduler : Scheduler.t
    }

  let create in_channel out_channel scheduler =
    let reader_ref = ref None in
    let t =
      let id = Id.gen () in
      { in_channel
      ; out_channel
      ; id
      ; reader = Async.create scheduler
      ; writer = Async.create scheduler
      ; scheduler
      }
    in
    reader_ref := Some t.reader;
    t

  let close t =
    (* TODO do this gently and wait for pending reads/writes *)
    t.scheduler.register_pending_ivar ();
    let ivar = Fiber.Ivar.create () in
    close_in_noerr t.in_channel;
    close_out_noerr t.out_channel;
    t.scheduler.on_event (Fill (ivar, ()));
    Fiber.Ivar.read ivar

  let read t =
    let+ res =
      Async.task t.reader ~f:(fun () -> Csexp.input_opt t.in_channel)
    in
    match res with
    | Error exn ->
      Async.stop t.reader;
      raise exn
    | Ok res -> (
      match res with
      | Ok (Some _ as s) -> s
      | Error _
      | Ok None ->
        Async.stop t.reader;
        None )

  let write t sexp =
    Async.task_exn t.writer ~f:(fun () ->
        Csexp.to_channel t.out_channel sexp;
        flush t.out_channel)
end

module Server = struct
  module type S = sig
    type t

    val create : Path.t -> backlog:int -> Scheduler.t -> t

    val serve : t -> Session.t Fiber.Sequence.t Fiber.t

    val stop : t -> unit
  end

  module type Transport = sig
    type t

    val create : Path.t -> backlog:int -> t

    val accept : t -> Unix.file_descr option

    val stop : t -> unit
  end

  module Make (T : Transport) : S = struct
    type t =
      { mutable transport : T.t option
      ; backlog : int
      ; scheduler : Scheduler.t
      ; path : Path.t
      }

    let create path ~backlog scheduler =
      { path; backlog; scheduler; transport = None }

    let serve (t : t) =
      let async = Async.create t.scheduler in
      let+ transport =
        Async.task_exn async ~f:(fun () -> T.create t.path ~backlog:t.backlog)
      in
      t.transport <- Some transport;
      let accept () =
        Async.task async ~f:(fun () ->
            T.accept transport
            |> Option.map ~f:(fun client ->
                   let in_ = Unix.in_channel_of_descr client in
                   let out = Unix.out_channel_of_descr client in
                   (in_, out)))
      in
      let rec loop () =
        let+ accept = accept () in
        match accept with
        | Error _
        | Ok None ->
          Fiber.Sequence.Nil
        | Ok (Some (in_, out)) ->
          let session = Session.create in_ out t.scheduler in
          Fiber.Sequence.Cons (session, loop ())
      in
      loop ()

    let stop t =
      match t.transport with
      | None -> Code_error.raise "server not running" []
      | Some t -> T.stop t
  end

  module Win : Transport = struct
    type t = Unix.file_descr

    let create path ~backlog:_ =
      let fd =
        Dune_named_pipe_stubs.Server.create (Path.to_absolute_filename path)
      in
      fd

    let accept t =
      Dune_named_pipe_stubs.Server.connect t;
      Some t

    let stop t =
      Dune_named_pipe_stubs.Server.disconnect t;
      Dune_named_pipe_stubs.Server.destroy t
  end

  module Unix : Transport = struct
    type t =
      { fd : Unix.file_descr
      ; sock : Path.t
      }

    let create sock ~backlog =
      let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      Path.unlink_no_err sock;
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      Path.mkdir_p (Path.parent_exn sock);
      Unix.bind fd (Unix.ADDR_UNIX (Path.to_absolute_filename sock));
      Unix.listen fd backlog;
      { fd; sock }

    let accept t =
      try
        let fd, _ = Unix.accept t.fd in
        Some fd
      with Unix.Unix_error (Unix.ECONNABORTED, _, _) -> None

    let stop { fd; sock } =
      (try Unix.close fd with _ -> ());
      Path.unlink_no_err sock
  end

  let transport : (module Transport) =
    if Sys.win32 then
      (module Win)
    else
      (module Unix)

  include (Make ((val transport)) : S)
end

module Client = struct
  module type S = sig
    type t

    val create : Path.t -> Scheduler.t -> t

    val stop : t -> unit

    val connect : t -> Session.t Fiber.t
  end

  module type Transport = sig
    type t

    val create : Path.t -> t

    val connect : t -> Unix.file_descr

    val close : t -> unit
  end

  module Make (T : Transport) : S = struct
    type t =
      { mutable transport : T.t option
      ; async : Async.t
      ; scheduler : Scheduler.t
      ; path : Path.t
      }

    let create path scheduler =
      let async = Async.create scheduler in
      { path; scheduler; async; transport = None }

    let connect t =
      Async.task_exn t.async ~f:(fun () ->
          let transport = T.create t.path in
          t.transport <- Some transport;
          let client = T.connect transport in
          let out = Unix.out_channel_of_descr client in
          let in_ = Unix.in_channel_of_descr client in
          Session.create in_ out t.scheduler)

    let stop t = Option.iter t.transport ~f:T.close
  end

  module Win = struct
    type t =
      { mutable fd : Unix.file_descr option
      ; path : Path.t
      }

    let close t = Option.iter t.fd ~f:Unix.close

    let create path = { path; fd = None }

    let connect t =
      Dune_named_pipe_stubs.Client.wait (Path.to_absolute_filename t.path) (-1);
      let fd =
        Dune_named_pipe_stubs.Client.openpipe (Path.to_absolute_filename t.path)
      in
      t.fd <- Some fd;
      fd
  end

  module Unix = struct
    type t =
      { fd : Unix.file_descr
      ; path : Path.t
      }

    let close t = Unix.close t.fd

    let create path =
      let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      { path; fd }

    let connect t =
      let () = Unix.connect t.fd (Unix.ADDR_UNIX (Path.to_string t.path)) in
      t.fd
  end

  let transport : (module Transport) =
    if Sys.win32 then
      (module Win)
    else
      (module Unix)

  include (Make ((val transport)) : S)
end
