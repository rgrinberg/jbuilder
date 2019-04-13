(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(*     Written by: Emilio Jesús Gallego Arias  *)

open! Stdune

(* This is in its own file due to dependency issues *)
type t

val make : Lib_name.t -> t

val pp : t Fmt.t

val to_sexp : t -> Sexp.t

module Map : Map.S with type key = t
