(** Set the main entry point of the program *)
val set_main : (unit -> unit) -> unit

(** Execute the registered entry point. Never returns *)
val run_main : unit -> _

(** Are we linked as part of a library test/benchmark runner or a normal
    executable? *)
val is_library_runner : unit -> bool
