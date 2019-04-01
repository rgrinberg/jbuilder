(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(*     Written by: Emilio Jesús Gallego Arias  *)

open! Stdune

type t

val name : t -> Lib_name.t

(* this is not really a wrapper for the prefix path *)
val wrapper : t -> string

val src_root : t -> Path.t
val obj_root : t -> Path.t

val package : t -> Package.Name.t option

module DB : sig

  type lib
  type t

  val create_from_coqlib_stanzas
    : (Path.t * Dune_file.Coq.t) list
    -> t

  val find_many
    :  t
    -> loc:Loc.t
    -> Lib_name.t list
    -> lib list Or_exn.t

  val resolve : t -> Loc.t * Lib_name.t -> lib Or_exn.t

end with type lib := t
