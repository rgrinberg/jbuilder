open Import

module Source : sig
  type t

  val make : dir:Path.Build.t -> loc:Loc.t -> main:string -> name:string -> t
  val loc : t -> Loc.t
end

type t

val setup_rules : t -> linkage:Exe.Linkage.t -> unit Memo.t

val make
  :  sctx:Super_context.t
  -> scope:Scope.t
  -> dir:Path.Build.t
  -> source:Source.t
  -> preprocess:Preprocess.Without_instrumentation.t Preprocess.t
  -> requires_compile:Lib.t list Resolve.Memo.t
  -> requires_link:Lib.t list Resolve.t Memo.Lazy.t
  -> Expander.t
  -> t Memo.t

type directives =
  { include_paths : Path.Set.t
  ; files_to_load : Path.t list
  ; uses : Path.t list
  ; pp : string option
  ; ppx : string option
  ; code : string list
  }

val print_toplevel_init_file : directives -> unit

module Stanza : sig
  val setup
    :  sctx:Super_context.t
    -> dir:Path.Build.t
    -> toplevel:Toplevel_stanza.t
    -> unit Memo.t
end
