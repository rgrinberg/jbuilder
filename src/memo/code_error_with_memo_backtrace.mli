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

val without_outer_callstack : Code_error.t -> t
