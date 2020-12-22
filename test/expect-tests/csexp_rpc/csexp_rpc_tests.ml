open! Stdune
open Csexp_rpc
open Fiber.O

let () = Dune_tests_common.init ()

type event =
  | Fill of Fiber.fill
  | Abort

module Scheduler : sig
  type t

  type error =
    | Abort
    | Never

  exception E of error

  val finally : t -> (unit -> unit) -> unit

  val create : unit -> t

  val run : t -> 'a Fiber.t -> 'a

  val for_csexp : t -> Csexp_rpc.Scheduler.t

  val push_event : t -> event -> unit
end = struct
  type error =
    | Abort
    | Never

  exception E of error

  let () =
    Printexc.register_printer (function
      | E Abort -> Some "E Abort"
      | E Never -> Some "E Never"
      | _ -> None)

  type t =
    { mutable pending_jobs : int
    ; finally : (unit -> unit) Queue.t
    ; completed : (Fiber.fill, error) result Queue.t
    ; mutex : Mutex.t
    ; cv : Condition.t
    }

  let finally t f = Queue.push t.finally f

  let push_event t (e : event) =
    Mutex.lock t.mutex;
    let event : (Fiber.fill, _) result option =
      match e with
      | Abort -> Some (Error Abort)
      | Fill fill -> Some (Ok fill)
    in
    t.pending_jobs <- t.pending_jobs - 1;
    assert (t.pending_jobs >= 0);
    if Queue.is_empty t.completed then Condition.signal t.cv;
    Option.iter event ~f:(Queue.push t.completed);
    Mutex.unlock t.mutex

  let register_pending_ivar t =
    Mutex.lock t.mutex;
    t.pending_jobs <- t.pending_jobs + 1;
    Mutex.unlock t.mutex

  let for_csexp t =
    let on_event e = push_event t (Fill e) in
    { Csexp_rpc.Scheduler.register_pending_ivar =
        (fun () -> register_pending_ivar t)
    ; on_event
    ; thread = Thread.create
    }

  let var : t Fiber.Var.t = Fiber.Var.create ()

  let create () =
    { pending_jobs = 0
    ; completed = Queue.create ()
    ; mutex = Mutex.create ()
    ; cv = Condition.create ()
    ; finally = Queue.create ()
    }

  let available t = not (Queue.is_empty t.completed)

  let next (t : t) =
    let loop () =
      while not (available t) do
        Condition.wait t.cv t.mutex
      done;
      Queue.pop_exn t.completed
    in
    loop ()

  let iter t () =
    Mutex.lock t.mutex;
    let res =
      match Queue.pop t.completed with
      | Some e -> e
      | None ->
        let count = t.pending_jobs in
        if count = 0 then
          Error Never
        else
          next t
    in
    Mutex.unlock t.mutex;
    match res with
    | Ok a -> a
    | Error e -> raise (E e)

  module Timeout : sig
    type t

    val create : float -> on_timeout:(unit -> unit) -> t

    val cancel : t -> unit
  end = struct
    type t =
      { mutable cancel : bool
      ; mutex : Mutex.t
      }

    let cancel t =
      Mutex.lock t.mutex;
      t.cancel <- true;
      Mutex.unlock t.mutex

    let loop (t, delay, on_timeout) =
      Thread.delay delay;
      let cancel =
        Mutex.lock t.mutex;
        let res = t.cancel in
        Mutex.unlock t.mutex;
        res
      in
      if not cancel then on_timeout ()

    let create delay ~on_timeout =
      let t = { cancel = false; mutex = Mutex.create () } in
      ignore (Thread.create loop (t, delay, on_timeout));
      t
  end

  let run (t : t) f =
    let timeout =
      Timeout.create 3.0 ~on_timeout:(fun () ->
          print_endline "scheduler execution timed out";
          register_pending_ivar t;
          push_event t Abort)
    in
    let fiber = Fiber.Var.set var t (fun () -> f) in
    let iter = iter t in
    Exn.protect
      ~f:(fun () -> Fiber.run fiber ~iter)
      ~finally:(fun () ->
        Timeout.cancel timeout;
        Queue.iter t.finally ~f:(fun f -> f ());
        Queue.clear t.finally;
        if t.pending_jobs <> 0 then
          Code_error.raise "non zero pending jobs"
            [ ("pending_jobs", Int t.pending_jobs) ])
end

let server scheduler where =
  let s = Scheduler.for_csexp scheduler in
  let server = Server.create where ~backlog:10 s in
  Scheduler.finally scheduler (fun () -> Server.stop server);
  server

let client scheduler where =
  let s = Scheduler.for_csexp scheduler in
  let client = Csexp_rpc.Client.create where s in
  Scheduler.finally scheduler (fun () -> Client.stop client);
  client

let%expect_test "csexp server life cycle" =
  let sock = Path.of_string "/tmp/foo.sock" in
  let scheduler = Scheduler.create () in
  let server = server scheduler sock in
  let sessions = Server.serve server in
  let client = client scheduler sock in
  let run =
    let* sessions = sessions in
    Fiber.fork_and_join_unit
      (fun () ->
        let* client = Client.connect client in
        let* () = Session.write client (List [ Atom "from client" ]) in
        print_endline "client: written";
        let* response = Session.read client in
        ( match response with
        | None -> print_endline "client: no response"
        | Some sexp ->
          Printf.printf "client: received %s\n" (Csexp.to_string sexp) );
        let+ () = Session.close client in
        print_endline "client: closed";
        Server.stop server)
      (fun () ->
        let+ () =
          Fiber.Sequence.parallel_iter sessions ~f:(fun session ->
              print_endline "server: received session";
              let* res = Csexp_rpc.Session.read session in
              match res with
              | None ->
                print_endline "server: session terminated";
                Fiber.return ()
              | Some csexp ->
                Printf.printf "server: received %s\n" (Csexp.to_string csexp);
                Session.write session (List [ Atom "from server" ]))
        in
        print_endline "server: sessions finished")
  in
  Scheduler.run scheduler run;
  [%expect
    {|
    server: received session
    client: written
    server: received (11:from client)
    client: received (11:from server)
    client: closed
    server: sessions finished |}]
