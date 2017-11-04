let runner = ref None

let set_main f = runner := (Some f)

let run_main () =
  begin match !runner with
  | None -> failwith "Runner not set"
  | Some f -> f ()
  end;
  exit 0

let is_library_runner () = false
