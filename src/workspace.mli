(** Workspaces definitions *)

open! Import

module Context : sig
  module Target : sig
    type t =
      | Native
      | Named of string
  end
  module Opam : sig
    type t =
      { name    : string
      ; profile : string
      ; switch  : string
      ; root    : string option
      ; merlin  : bool
      ; targets : Target.t list
      }
  end

  module Default : sig
    type t =
      { profile : string
      ; targets : Target.t list
      }
  end

  type t = Default of Default.t | Opam of Opam.t

  val name : t -> string
end

type t =
  { merlin_context : string option
  ; contexts       : Context.t list
  }

val load : ?x:string -> ?profile:string -> Path.t -> t
