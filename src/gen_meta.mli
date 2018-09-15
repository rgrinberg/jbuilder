(** Generate a META file *)

open! Import

val gen
  :  package:string
  -> version:string option
  -> Lib.t list
  -> Dfindlib.Meta.t
