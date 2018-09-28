open Import

type t

val make
  :  ocaml_config:Value.t list String.Map.t
  -> pforms:Pform.Map.t
  -> pkg_version:Pkg_version.t
  -> artifacts:Artifacts.t
  -> host_artifacts:Artifacts.t
  -> t

val add_bindings : t -> Pform.Map.t -> t

val bindings : t -> Pform.Map.t

module Static : sig
  val expand_vars
    :  t
    -> mode:'a String_with_vars.Mode.t
    -> scope:Scope.t
    -> dir:Path.t
    -> ?bindings:Pform.Map.t
    -> String_with_vars.t
    -> 'a

  val expand_vars_string
    :  t
    -> scope:Scope.t
    -> dir:Path.t
    -> ?bindings:Pform.Map.t
    -> String_with_vars.t
    -> string

  val expand_vars_path
    :  t
    -> scope:Scope.t
    -> dir:Path.t
    -> ?bindings:Pform.Map.t
    -> String_with_vars.t
    -> Path.t
end

module Dynamic : sig
  module Resolved_forms : sig
    type t
    (* Failed resolutions *)
    val failures : t -> fail list

    (* All "name" for %{lib:name:...}/%{lib-available:name} forms *)
    val lib_deps : t -> Lib_deps_info.t

    (* Static deps from %{...} variables. For instance %{exe:...} *)
    val sdeps    : t -> Path.Set.t

    (* Dynamic deps from %{...} variables. For instance %{read:...} *)
    val ddeps    : t -> (unit, Value.t list) Build.t String.Map.t
  end

  type targets =
    | Static of Path.t list
    | Infer
    | Alias (** This action is for an alias *)

  type expander

  val with_expander
    :  expander
    -> dir:Path.t
    -> dep_kind:Lib_deps_info.Kind.t
    -> scope:Scope.t
    -> targets_written_by_user:targets
    -> map_exe:(Path.t -> Path.t)
    -> f:(Value.t list option String_with_vars.expander -> 'a)
    -> 'a * Resolved_forms.t
end with type expander := t

val expand_and_eval_set
  :  t
  -> scope:Scope.t
  -> dir:Path.t
  -> ?bindings:Pform.Map.t
  -> Ordered_set_lang.Unexpanded.t
  -> standard:(unit, string list) Build.t
  -> (unit, string list) Build.t
