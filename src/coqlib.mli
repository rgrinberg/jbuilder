(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018                    *)
(*     Written by: Emilio Jesús Gallego Arias  *)

(* Build rules for Coq's .v -> .vo files       *)

open! Stdune

module CoqModule : sig
  type t
  val make : file:Path.t -> t
  (* file = .v source file; module name has to be the same so far *)
  val source : t -> Path.t
  val name : t -> string
  val obj_file : obj_dir:Path.t -> ext:string -> t -> Path.t
  val pp : Format.formatter -> t -> unit
end

val build_coq_modules
  :  modules:Ordered_set_lang.t
  -> dir:Path.t
  -> dir_contents:Dir_contents.t
  -> CoqModule.t list

val gen_rules
  :  sctx:Super_context.t
  -> dir:Path.t
  -> dir_contents:Dir_contents.t
  -> scope:Scope.t
  -> Dune_file.Coq.t
  -> (unit, Action.t) Build.t list

val install_rules
  :  sctx:Super_context.t
  -> dir:Path.t
  -> Dune_file.Coq.t
  -> Install.Entry.t list
