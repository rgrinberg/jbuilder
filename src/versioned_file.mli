(** Implementation of versioned files *)

open Stdune

module type S = sig
  type data

  module Lang : sig

    (** [register id data] registers a new language. Users will select
        this language by writing:

        {[ (lang <name> <version>) ]}

        as the first line of the versioned file. *)
    val register : Syntax.t -> data -> unit

    module Instance : sig
      type t =
        { syntax  : Syntax.t
        ; data    : data
        ; version : Syntax.Version.t
        }
    end

    (** Return the latest version of a language. *)
    val get_exn : string -> Instance.t
  end

  (** [load fn ~f] loads a versioned file. It parses the first line,
      looks up the language, checks that the version is supported and
      parses the rest of the file with [f]. *)
  val load : Path.t -> f:(Lang.Instance.t -> 'a Sexp.Of_sexp.t) -> 'a
end

module Make(Data : sig type t end) : S with type data := Data.t
