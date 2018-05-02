(** Workspaces definitions *)

open! Import

module Context : sig
  module Coverage : sig
    type t = Coverage0.t =
      { coverage     : string
      ; covered_dirs : Ordered_set_lang.t
      }
  end
  module Target : sig
    type t =
      | Native
      | Named of string
  end
  module Opam : sig
    type t =
      { name         : string
      ; switch       : string
      ; root         : string option
      ; merlin       : bool
      ; targets      : Target.t list
      ; coverage     : Coverage.t option
      }
  end
  module Default : sig
    type t =
      { targets      : Target.t list
      ; coverage     : Coverage.t option
      }

    val create : ?coverage:Coverage.t -> targets:Target.t list -> unit -> t
  end

  type t = Default of Default.t | Opam of Opam.t

  val default : t

  val name : t -> string
end

type t =
  { merlin_context : string option
  ; contexts       : Context.t list
  }

val default : t

val load : ?x:string -> Path.t -> t
