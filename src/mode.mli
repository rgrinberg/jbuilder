open! Import

type t = Byte | Native

val decode : t Dune_lang.Decoder.t

val all : t list

val compiled_unit_ext : t -> string
val compiled_lib_ext : t -> string
val exe_ext : t -> string
val plugin_ext : t -> string

val cm_kind : t -> Cm_kind.t
val of_cm_kind : Cm_kind.t -> t

val variant : t -> Variant.t

val pp : t Fmt.t

module Dict : sig
  type mode = t

  type 'a t =
    { byte   : 'a
    ; native : 'a
    }

  val pp : 'a Fmt.t -> 'a t Fmt.t

  val encode : 'a Dune_lang.Encoder.t -> 'a t Dune_lang.Encoder.t
  val decode : 'a Dune_lang.Decoder.t -> 'a t Dune_lang.Decoder.t

  val get : 'a t -> mode -> 'a

  val of_func : (mode:mode -> 'a) -> 'a t

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val make_both : 'a -> 'a t

  module Set : sig

    type nonrec t = bool t
    val decode : t Dune_lang.Decoder.t
    val all : t
    val is_empty : t -> bool
    val to_list : t -> mode list
    val of_list : mode list -> t
    val iter : t -> f:(mode -> unit) -> unit
  end
end with type mode := t
