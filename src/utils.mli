(** Utilities that can't go in [Import] *)

open! Import

(** Return the absolute path to the shell and the argument to pass it (-c or /c). Raise in
    case in cannot be found. *)
val system_shell_exn : needed_to:string -> Path.t * string

(** Same as [system_shell_exn] but for bash *)
val bash_exn : needed_to:string -> Path.t

(** Convert a signal number to a name: INT, TERM, ... *)
val signal_name : int -> string

(** Return the path to the jbuild file in this directory as a string. *)
val jbuild_name_in : dir:Path.t -> string

(** Nice description of a target *)
val describe_target : Path.t -> string

(** Raise an error about a program not found in the PATH or in the tree *)
val program_not_found
  :  ?context:string
  -> ?hint:string
  -> string
  -> _

(** Raise an error about a library not found *)
val library_not_found : ?context:string -> ?hint:string -> string -> _

(** [\["-g"\]] if [!Clflags.g] and [\[\]] otherwise *)
val g : unit -> string list

(** Similar to [String_map.find] but with a better error message in case of
    failure. *)
val find_module : dir:Path.t -> 'a String_map.t -> string -> 'a
val find_deps   : dir:Path.t -> 'a String_map.t -> string -> 'a

(** Base name of the object file (.o) for a given source file basename:

    - [obj_name_of_basename "toto.ml" = "toto"]
    - [obj_name_of_basename "toto.pp.ml" = "toto"]
*)
val obj_name_of_basename : string -> string

val install_file : package:string -> findlib_toolchain:string option -> string

(** Digest files with caching *)
module Cached_digest : sig
  (** Digest the contents of the following file *)
  val file : Path.t -> Digest.t

  (** Clear the following digest from the cache *)
  val remove : Path.t -> unit

  (** Dump/load the cache to/from the disk *)
  val dump : unit -> unit
  val load : unit -> unit
end
