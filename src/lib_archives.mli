open Stdune

type t

val make
  :  ctx:Context.t
  -> dir:Path.t
  -> Dune_file.Library.t
  -> t

val install_entries : t -> Install.Entry.t list

val all : t -> Path.Set.t
