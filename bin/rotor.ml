open! Stdune
open! Import

let doc = "Run rotor command with appropriate flags"

let man =
  [ `S "DESCRIPTION"
  ; `P {|$(b,dune rotor ARGS) run rotor with ARGS on the current workspace|}
  ]

let info = Term.info "rotor" ~doc ~man

let term =
  let+ common = Common.term
  and+ args = Arg.(value & pos_right 0 string [] (Arg.info [] ~docv:"ARGS"))
  and+ ctx_name =
    Common.context_arg ~doc:{|Select context to use for build artifacts.|}
  in
  Common.set_dirs common;
  let log = Log.create common in
  let run () =
    let open Fiber.O in
    let* setup = Import.Main.setup ~log common in
    let sctx = String.Map.find_exn setup.Import.Main.scontexts ctx_name in
    let targets =
      let dir = Path.(relative root) (Common.prefix_target common ".") in
      [ Target.Alias (Alias.in_dir ~name:"check" ~recursive:true
                        ~contexts:setup.workspace.contexts dir)
      ]
    in
    let* () = do_build setup targets in
    let* () = Dune.Rotor.run sctx ~args in
    do_build setup targets
  in
  Scheduler.go ~log ~common run;
  Hooks.End_of_build.run ()

let command = term, info
