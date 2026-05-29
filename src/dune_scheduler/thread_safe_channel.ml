open Import
open Fiber.O

type 'a item =
  | Value of 'a
  | Fills of Fiber.fill list

type 'a t =
  { events : Event.Queue.t
  ; items : 'a item Queue.t
  ; readers : 'a option Fiber.Ivar.t Queue.t
  ; mutex : Mutex.t
  ; mutable closed : bool
  }

let create events =
  { events
  ; items = Queue.create ()
  ; readers = Queue.create ()
  ; mutex = Mutex.create ()
  ; closed = false
  }
;;

let send_fills t fills = Event.Queue.send_worker_tasks_completed t.events fills

let write t value =
  let status, fill =
    Mutex.protect t.mutex (fun () ->
      if t.closed
      then `Closed, None
      else (
        match Queue.pop t.readers with
        | Some ivar -> `Ok, Some (Fiber.Fill (ivar, Some value))
        | None ->
          Queue.push t.items (Value value);
          `Ok, None))
  in
  Option.iter fill ~f:(fun fill -> send_fills t [ fill ]);
  status
;;

let raise_if_closed = function
  | `Closed -> Code_error.raise "Thread_safe_channel is closed" []
  | `Ok -> ()
;;

let write_exn t value = write t value |> raise_if_closed

let write_fills t fills =
  let status, fills =
    Mutex.protect t.mutex (fun () ->
      if t.closed
      then `Closed, []
      else if List.is_empty fills
      then `Ok, []
      else if Queue.is_empty t.readers
      then (
        Queue.push t.items (Fills fills);
        `Ok, [])
      else `Ok, fills)
  in
  send_fills t fills;
  status
;;

let write_fills_exn t fills = write_fills t fills |> raise_if_closed

let rec read t =
  let* () = Fiber.return () in
  match
    Mutex.protect t.mutex (fun () ->
      match Queue.pop t.items with
      | Some (Value value) -> `Ready (Some value)
      | Some (Fills fills) -> `Fills fills
      | None when t.closed -> `Ready None
      | None ->
        let ivar = Fiber.Ivar.create () in
        Queue.push t.readers ivar;
        `Wait ivar)
  with
  | `Ready value -> Fiber.return value
  | `Fills fills ->
    send_fills t fills;
    read t
  | `Wait ivar -> Fiber.Ivar.read ivar
;;

let close t =
  let fills =
    Mutex.protect t.mutex (fun () ->
      if t.closed
      then []
      else (
        t.closed <- true;
        let rec drain acc =
          match Queue.pop t.readers with
          | None -> List.rev acc
          | Some ivar -> drain (Fiber.Fill (ivar, None) :: acc)
        in
        drain []))
  in
  send_fills t fills
;;
