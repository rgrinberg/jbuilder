(** [Ordered_set_lang.t] is a sexp-based representation for an ordered list of strings,
    with some set like operations. *)

open Import

type t
val t : t Sexp.Of_sexp.t

(** Return the location of the set. [loc standard] returns [None] *)
val loc : t -> Loc.t option

(** Value parsed from elements in the DSL *)
module type Value = sig
  type t
  type key
  val key : t -> key
end

module type Key = sig
  type t
  val compare : t -> t -> Ordering.t
  module Map : Map.S with type key = t
end

module type S = sig
  (** Evaluate an ordered set. [standard] is the interpretation of [:standard]
      inside the DSL. *)
  type value
  type 'a map

  val eval
    :  t
    -> parse:(loc:Loc.t -> string -> value)
    -> standard:value list
    -> value list

  (** Same as [eval] but the result is unordered *)
  val eval_unordered
    :  t
    -> parse:(loc:Loc.t -> string -> value)
    -> standard:value map
    -> value map
end

module Make(Key : Key)(Value : Value with type key = Key.t)
  : S with type value = Value.t
       and type 'a map = 'a Key.Map.t

val standard : t
val is_standard : t -> bool

val field : ?default:t -> string -> t Sexp.Of_sexp.fields_parser

module Partial : sig
  type expanded = t
  type t

  val expand
    :  t
    -> dir:Path.t
    -> files_contents:Sexp.Ast.t Path.Map.t
    -> f:(String_with_vars.t -> Value.t list)
    -> expanded
end with type expanded := t

module Unexpanded : sig
  type expanded = t
  type t

  include Sexp.Sexpable with type t := t
  val standard : t

  val field : ?default:t -> string -> t Sexp.Of_sexp.fields_parser

  val has_special_forms : t -> bool

  type expander =
    { f: 'a. mode:'a String_with_vars.Mode.t
        -> String_with_vars.t
        -> ('a, String_with_vars.t) String_with_vars.Partial.t
    }

  val expand
    :  t
    -> dir:Path.t
    -> f:expander
    -> Partial.t * Sexp.syntax * Path.Set.t

  type position = Pos | Neg

  (** Fold a function over all strings in a set. The callback receive
      whether the string is in position or negative position, i.e. on
      the left or right of a [\] operator. *)
  val fold_strings
    :  t
    -> init:'a
    -> f:(position -> String_with_vars.t -> 'a -> 'a)
    -> 'a
end with type expanded := t

module String : S with type value = string and type 'a map = 'a String.Map.t
