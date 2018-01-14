let runner = ref None

let set_main f =
  match !runner with
  | None ->
    runner := Some f
  | Some _ ->
    failwith "Libmain.set_main: entry point already set"

let run_main () =
  match !runner with
  | None ->
    failwith "Libmain.run_main: entry point not set"
  | Some f ->
    f ();
    exit 0
