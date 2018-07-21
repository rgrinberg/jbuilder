(** A directory with a jbuild *)

open Stdune

type t =
  { src_dir : Path.t
  ; ctx_dir : Path.t
  ; stanzas : Dune_file.Stanzas.t
  ; scope   : Scope.t
  ; kind    : File_tree.Dune_file.Kind.t
  }
