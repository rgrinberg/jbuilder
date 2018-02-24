open! Import

include module type of struct include Sub_system_intf end

module Register_backend(M : Backend) : Registered_backend with type t = M.t

module Register_with_backend(M : With_backend) : sig end

(** Scan the sub-systems used by the library and generate rules for
    all of the ones that needs it. *)
val gen_rules : Library_compilation_context.t -> unit
