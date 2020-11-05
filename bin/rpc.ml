open! Stdune
open Import

let doc = "Dune's RPC mechanism. Experimental."

let man =
  [ `S "DESCRIPTION"
  ; `P {|This is experimental. do not use|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "rpc" ~doc ~man

let term =
  let+ (common : Common.t) = Common.term in
  let config = Common.config common in
  let config = { config with rpc = Some Client } in
  let common = Common.set_config common config in
  Common.set_common common ~targets:[] ~external_lib_deps_mode:false;
  match Dune_rpc.where () with
  | None -> User_error.raise [ Pp.text "not running" ]
  | Some p ->
    Dune_engine.Scheduler.rpc_client ~config p (fun c ->
        let open Fiber.O in
        let* session = Csexp_rpc.Client.connect c in
        let stdio = Dune_engine.Scheduler.connect_rpc stdin stdout in
        let rec forever f =
          let* () = f () in
          forever f
        in
        let forward f t =
          forever (fun () ->
              let* csexp = Csexp_rpc.Session.read f in
              match csexp with
              | None -> Csexp_rpc.Session.close t
              | Some csexp -> Csexp_rpc.Session.write t csexp)
        in
        Fiber.fork_and_join_unit
          (fun () -> forward session stdio)
          (fun () -> forward stdio session))

let command = (term, info)
