(** Set the main entry point of the program *)
val set_main : (unit -> unit) -> unit

(** Execute the registered entry point. Never returns *)
val run_main : unit -> _
