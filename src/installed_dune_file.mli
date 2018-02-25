(** Dune files that are installed on the system *)

val load : fname:string -> Jbuild.Sub_system_info.t Sub_system_name.Map.t
val gen : Sexp.t Sub_system_name.Map.t -> Sexp.t
