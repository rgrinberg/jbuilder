open Stdune
open Dune_engine
open! Fiber.O
open! Dune_tests_common

let () = init ()

let config handler =
  let dir = Path.build Path.Build.root in
  let rpc = Some (Config.Rpc.Server { dir; handler; backlog = 10 }) in
  { Config.default with rpc }

let%expect_test "initialize scheduler with rpc" =
  let handler =
    Dune_rpc.Handler.create
      ~on_request:(fun _ _ -> assert false)
      ~on_notification:(fun _ _ -> assert false)
      ~on_init:(fun _ -> assert false)
  in
  let config = config handler in
  Scheduler.go ~config Fiber.return;
  [%expect {||}]
