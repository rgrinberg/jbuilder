open Import

type rule =
  { exe: Jbuild.Executables.t
  ; alias_name: string
  ; alias_action: (unit, Action.t) Build.t
  ; alias_stamp: Sexp.t
  ; all_modules: Module.t String_map.t
  ; gen_source : (unit, Action.t) Build.t
  }

val make_rules
  : Super_context.PP.Ppx_info.t
  -> sctx:Super_context.t
  -> dir:Path.t
  -> lib:Jbuild.Library.t
  -> scope:Jbuild.Scope.t
  -> rule

val setup_rules
  : Super_context.t
  -> lib:Jbuild.Library.t
  -> dir:Path.t
  -> scope:Jbuild.Scope.t
  -> (unit, rule option) Build.t
