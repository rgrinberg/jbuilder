(** Where libraries are

    This module is used to implement [Super_context.Libs].
*)

open Import

type t

module Scope : sig
  type t

  val find : t -> string -> Lib.t option
  val find_exn : t -> string -> Lib.t

  val root : t -> Path.t
  val resolve : t -> string -> (Package.t, string) result
end

val create
  :  Findlib.t
  -> scopes:Jbuild.Scope.t list
  -> root:Path.t
  -> (Path.t * Jbuild.Library.t) list
  -> t

val find     : t -> from:Path.t -> string -> Lib.t option
val find_exn : t -> from:Path.t -> string -> Lib.t

val internal_libs_without_non_installable_optional_ones : t -> Lib.Internal.t list

val interpret_lib_deps
  :  t
  -> dir:Path.t
  -> Jbuild.Lib_dep.t list
  -> Lib.Internal.t list * Findlib.package list * fail option

val best_lib_dep_names_exn
  :  t
  -> dir:Path.t
  -> Jbuild.Lib_dep.t list
  -> string list

(** [all_ppx_runtime_deps_exn t ~dir deps] takes the transitive closure of [deps] and
    return the set of all the ppx runtime dependencies of these libraries. *)
val all_ppx_runtime_deps_exn
  :  t
  -> dir:Path.t
  -> Jbuild.Lib_dep.t list
  -> String_set.t

type resolved_select =
  { src_fn : string
  ; dst_fn : string
  }

val resolve_selects
  :  t
  -> from:Path.t
  -> Jbuild.Lib_dep.t list
  -> resolved_select list

val lib_is_available : t -> from:Path.t -> string -> bool

(** For [Findlib.closure] *)
val local_public_libs : t -> Path.t String_map.t

(** Unique name, even for internal libraries *)
val unique_library_name : t -> Lib.t -> string

val find_scope : t -> dir:Path.t -> Scope.t

val anonymous : t -> Scope.t
val external_ : t -> Scope.t
