(* This open is unused with OCaml >= 4.03 since the stdlib defines a result type *)
[@@@warning "-33"]
open Result_compat
open Pervasives
[@@@warning "+33"]

(** The result type.

    It is equal to the result type defined by the standard library for OCaml >= 4.03.
*)
type ('a, 'error) t = ('a, 'error) result =
  | Ok    of 'a
  | Error of 'error

(** Map successful results *)
val map : ('a, 'error) t -> f:('a -> 'b) -> ('b, 'error) t

(** For compatibility with some other code *)
type ('a, 'error) result = ('a, 'error) t
