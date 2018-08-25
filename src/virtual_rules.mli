open! Stdune

module Implementation : sig
  type t

  val vlib_stubs_o_files : t -> dir:Path.t -> Path.t list

  val dep_graph : t -> Ocamldep.Dep_graphs.t -> Ocamldep.Dep_graphs.t

  val add_vlib_modules
    :  t
    -> Module.t Module.Name.Map.t
    -> Module.t Module.Name.Map.t
end

module Gen (S : sig val sctx : Super_context.t end) : sig

  (** Copy over .o/.cm[oix] files to the obj dir impl from vlib *)
  val setup_copy_rules_for_impl : dir:Path.t -> Implementation.t -> unit

  val implements_rules
    :  lib:Dune_file.Library.t
    -> scope:Scope.t -> modules:Module.t Module.Name.Map.t
    -> Loc.t * string
    -> Implementation.t
end
