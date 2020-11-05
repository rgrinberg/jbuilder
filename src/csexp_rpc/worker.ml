open Stdune

type thread = { create : 'a 'b. ('a -> 'b) -> 'a -> Thread.t }

let with_mutex t ~f =
  Mutex.lock t;
  let res = f () in
  Mutex.unlock t;
  res

type state =
  | Running of Thread.t
  | Stopped of Thread.t
  | Finished

type 'a t =
  { work : 'a Queue.t
  ; mutable state : state
  ; mutex : Mutex.t
  ; work_available : Condition.t
  }

let is_running t =
  match t.state with
  | Running _ -> true
  | Stopped _
  | Finished ->
    false

let run (f, t) =
  let rec loop () =
    match t.state with
    | Stopped _ -> (
      match Queue.pop t.work with
      | None -> t.state <- Finished
      | Some job -> do_work job )
    | Finished -> ()
    | Running _ -> (
      match Queue.pop t.work with
      | Some job -> do_work job
      | None ->
        while Queue.is_empty t.work && is_running t do
          Condition.wait t.work_available t.mutex
        done;
        loop () )
  and do_work job =
    Mutex.unlock t.mutex;
    f job;
    Mutex.lock t.mutex;
    loop ()
  in
  Mutex.lock t.mutex;
  loop ();
  Mutex.unlock t.mutex

let create { create } do_ =
  let t =
    { work = Queue.create ()
    ; state = Finished
    ; mutex = Mutex.create ()
    ; work_available = Condition.create ()
    }
  in
  t.state <- Running (create run (do_, t));
  t

let add_work (type a) (t : a t) (w : a) =
  with_mutex t.mutex ~f:(fun () ->
      if is_running t then (
        Queue.push t.work w;
        Condition.signal t.work_available;
        Ok ()
      ) else
        Error `Stopped)

let stop (t : _ t) =
  with_mutex t.mutex ~f:(fun () ->
      match t.state with
      | Running th ->
        t.state <- Stopped th;
        Condition.signal t.work_available
      | Stopped _
      | Finished ->
        ())
