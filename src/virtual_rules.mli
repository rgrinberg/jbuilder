open! Stdune

module Gen (S : sig val sctx : Super_context.t end) : sig

  (** Copy over .o/.cm[oix] files to teh obj dir impl from vlib *)
  val setup_copy_rules_for_impl
    :  dir:Path.t
    -> impl:Dune_file.Library.t
    -> vlib:Lib.t
    -> unit

  val implements_rules
    :  lib:Dune_file.Library.t
    -> scope:Scope.t -> modules:Module.t Module.Name.Map.t
    -> Loc.t * string
    -> unit
end
