open Import

type 'a t =
  { data : 'a
  ; required_by : entry list
  }

and entry =
  | Path of Path.t
  | Virt of string

exception E of exn * entry list

let reraise exn entry =
  reraise (
    match exn with
    | E (exn, entries) -> E (exn, entry :: entries)
    | exn -> E (exn, [entry]))

let unwrap_exn = function
  | E (exn, entries) -> (exn, Some entries)
  | exn -> (exn, None)

let () =
  Printexc.register_printer (function
    | E (exn, _) -> Some (Printexc.to_string exn)
    | _ -> None)
