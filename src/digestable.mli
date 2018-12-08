open Stdune

module For_digest : sig
  type t
end

type 'a t = 'a -> For_digest.t

val raw : 'a t

val string : string t

val opt : 'a t -> 'a option t

val string_opt : string option t

val int : int t

val float : float t

val bool : bool t

val list : 'a t -> 'a list t

val path : Path.t t

val env_vars : (Env.t * Env.Var.Set.t) t

val string_set : String.Set.t t

val paths : Path.t list t

val trace_paths : Path.t list t
val trace_paths_set : Path.Set.t t

val digest : 'a t -> 'a -> Digest.t

val to_string : 'a t -> 'a -> string

val t2 : 'a t -> 'b t -> ('a * 'b) t
val t3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val t4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
