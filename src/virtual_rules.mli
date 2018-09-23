open Stdune

module Implementation : sig
  type t

  val dep_graph
    :  t
    -> Ocamldep.Dep_graphs.t
    -> Ocamldep.Dep_graph.t Ml_kind.Dict.t

  val modules_of_vlib : t -> Module.Name_map.t
end

module Gen (S : sig val sctx : Super_context.t end) : sig
  val vlib_stubs_o_files : Implementation.t -> Path.t list

  val setup_copy_rules_for_impl
    :  dir:Path.t
    -> Implementation.t
    -> unit

  val impl
    :  lib:Dune_file.Library.t
    -> scope:Scope.t
    -> source_modules:Module.Name_map.t
    -> Implementation.t option
end
