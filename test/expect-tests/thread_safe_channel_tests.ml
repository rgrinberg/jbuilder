open Stdune
include Dune_scheduler
open Dune_tests_common
open Fiber.O
module Thread_safe_channel = Dune_scheduler.For_tests.Thread_safe_channel

let () = init ()

let%expect_test "wakes fibers from side threads" =
  let event_queue = Event.Queue.create () in
  let channel = Thread_safe_channel.create event_queue in
  ignore
    (Thread.create
       (fun () ->
          for i = 1 to 3 do
            match Thread_safe_channel.write channel i with
            | `Ok -> ()
            | `Closed -> print_endline "channel closed"
          done;
          Thread_safe_channel.close channel)
       ());
  let rec read_all acc =
    let* value = Thread_safe_channel.read channel in
    match value with
    | None -> Fiber.return (List.rev acc)
    | Some value -> read_all (value :: acc)
  in
  let iter () =
    match Event.Queue.next event_queue with
    | Fiber_fill_ivar fill -> [ fill ]
    | Shutdown _ | Job_complete_ready -> assert false
  in
  Fiber.run (read_all []) ~iter |> Dyn.list Dyn.int |> print_dyn;
  [%expect {| [ 1; 2; 3 ] |}]
;;
