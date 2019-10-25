open Stdune

(* A single memo frame and the OCaml frames it called which lead to the error *)
type frame =
  { ocaml : string
  ; memo : Dyn.t
  }

type t =
  { exn : Code_error.t
  ; reverse_backtrace : frame list
  ; (* [outer_call_stack] is a trick to capture some of the information
       that's lost by the async memo error handler. It can be safely ignored
       by the sync error handler. *)
    outer_call_stack : Dyn.t
  }

type exn += E of t

let frame_to_dyn { ocaml; memo } =
  Dyn.Record [ ("ocaml", Dyn.String ocaml); ("memo", memo) ]

let to_dyn { exn; reverse_backtrace; outer_call_stack } =
  Dyn.Record
    [ ("exn", Code_error.to_dyn exn)
    ; ( "backtrace"
      , Dyn.Encoder.list frame_to_dyn (List.rev reverse_backtrace) )
    ; ("outer_call_stack", outer_call_stack)
    ]

let without_outer_callstack exn =
  { exn
  ; reverse_backtrace = []
  ; outer_call_stack = Dyn.String "<n/a>"
  }

let () =
  Printexc.register_printer (function
    | E t -> Some (Dyn.to_string (to_dyn t))
    | _ -> None)
