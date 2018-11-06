open! Stdune

type t

val decode : t Stanza.Decoder.t

val empty : t

val filter : t -> standard:t -> string list Lazy.t -> string list
