open Format

type 'a t = formatter -> 'a -> unit

val failwith : ('a, formatter, unit, 'b) format4 -> 'a

val string : string -> formatter -> unit

val nl : unit t

val prefix : (formatter -> unit) -> 'a t -> 'a t

val ocaml_list : 'a t -> 'a list t

val quoted : string t

val const : 'a t -> 'a -> unit t

val record : (string * unit t) list t

val tuple : 'a t -> 'b t -> ('a * 'b) t

val list : ?pp_sep:unit t -> 'a t -> 'a list t

val kstrf : (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
