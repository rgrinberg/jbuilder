(** OCaml module compilation *)

open Import

module Includes : sig
  type t

  val make : Super_context.t -> requires:Lib.t list Or_exn.t -> t

  (** Empty set of include directories *)
  val empty : t
end

(** Setup rules to build a single module. *)
val build_module
  :  ?sandbox:bool
  -> ?js_of_ocaml:Jbuild.Js_of_ocaml.t
  -> ?dynlink:bool
  -> dep_graphs:Ocamldep.Dep_graphs.t
  -> Compilation_context.t
  -> Module.t
  -> unit

(** Setup rules to build all of the modules in the compilation context. *)
val build_modules
  :  ?sandbox:bool
  -> ?js_of_ocaml:Jbuild.Js_of_ocaml.t
  -> ?dynlink:bool
  -> dep_graphs:Ocamldep.Dep_graphs.t
  -> Compilation_context.t
  -> unit
