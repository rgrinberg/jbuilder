open Import

module Internal : sig
  type t = Path.t * Jbuild.Library.t
end

type t =
  | Internal of Internal.t * bool
  | External of Findlib.package * bool

module Set : Set.S with type elt := t

val included : t -> bool
val set_included : t -> t
val unset_included : t -> t

val lib_obj_dir : Path.t -> Jbuild.Library.t -> Path.t

(*val deps : t -> string list*)

val include_paths : t list -> Path.Set.t

val include_flags : t list -> _ Arg_spec.t

val c_include_flags : t list -> _ Arg_spec.t

val link_flags : t list -> mode:Mode.t -> _ Arg_spec.t

val archive_files : t list -> mode:Mode.t -> ext_lib:string -> Path.t list

val jsoo_runtime_files : t list -> Path.t list

(** [public_name] if present, [name] if not *)
val best_name : t -> string

val describe : t -> string

val remove_dups_preserve_order : t list -> t list

(*val ppx_runtime_libraries : t list -> String_set.t
*)
