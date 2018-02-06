(** Configuration parameters *)

open! Import

(** Local installation directory *)
val local_install_dir : context:string -> Path.t

val local_install_bin_dir : context:string -> Path.t
val local_install_man_dir : context:string -> Path.t
val local_install_lib_dir : context:string -> package:string -> Path.t

val dev_null : Path.t

(** When this file is present in a directory jbuilder will delete
    nothing in it if it knows to generate this file. *)
val jbuilder_keep_fname : string

(** Jbuilder configuration *)

module Display : sig
  type t =
    | Progress (** Single interactive status line *)
    | Short    (** One line per command           *)
    | Verbose  (** Display all commands fully     *)
    | Quiet    (** Only display errors            *)

  val t : t Sexp.Of_sexp.t
  val all : t list
  val of_string : string -> t option
  val to_string : t -> string
end

type t =
  { display : Display.t
  }

val t : t Sexp.Of_sexp.t

val default : t
val user_config_file : string
val load_user_config_file : unit -> t
val load_config_file : fname:string -> t
