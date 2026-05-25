open Stdune
include Dune_scheduler
open Dune_tests_common
open Dune_engine
open Fiber.O
module Thread_safe_channel = Dune_scheduler.For_tests.Thread_safe_channel

let () = init ()

let config =
  Clflags.display := Short;
  { Scheduler.Config.concurrency = 1
  ; print_ctrl_c_warning = false
  ; watch_exclusions = []
  }
;;

let go ?(timeout = Time.Span.of_secs 0.3) f =
  try
    Scheduler.Run.go ~timeout config ~file_watcher:No_watcher ~on_event:(fun _ -> ()) f
  with
  | Shutdown.E Requested -> ()
;;

let%expect_test "wakes fibers from side threads" =
  go (fun () ->
    let channel = Thread_safe_channel.create_for_current_scheduler () in
    ignore
      (Scheduler.spawn_thread ~name:"channel-test-producer" (fun () ->
         for i = 1 to 3 do
           match Thread_safe_channel.write channel i with
           | `Ok -> ()
           | `Closed -> print_endline "channel closed"
         done;
         Thread_safe_channel.close channel));
    let rec read_all acc =
      let* value = Thread_safe_channel.read channel in
      match value with
      | None -> Fiber.return (List.rev acc)
      | Some value -> read_all (value :: acc)
    in
    let* values = read_all [] in
    print_dyn (Dyn.list Dyn.int values);
    Fiber.return ());
  [%expect {| [ 1; 2; 3 ] |}]
;;
