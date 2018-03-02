(** Odoc rules *)

open Jbuild

val setup_package_aliases : Super_context.t -> Package.t -> unit

val setup_library_odoc_rules
  :  Super_context.t
  -> Library.t
  -> dir:Path.t
  -> scope:Scope.t
  -> modules:Module.t Module.Name.Map.t
  -> requires:(unit, Lib.t list) Build.t
  -> dep_graphs:Ocamldep.Dep_graphs.t
  -> unit

val gen_rules : Super_context.t -> dir:Path.t -> string list -> unit

val pkg_odoc : Super_context.t -> Package.t -> Path.t

val setup_package_odoc_rules
  :  Super_context.t
  -> pkg:Package.t
  -> mlds:Path.t list
  -> entry_modules_by_lib:(Lib.t -> Module.t list)
  -> unit
