(** Versionning of syntactic elements *)

(** A syntax version.

    It is always assumed that a parser with version [(X, Y)] can read
    the output produced by a printer at version [(X, Z)] for any [Z <=
    Y]. *)
type t = int * int

val sexp_of_t : t Sexp.To_sexp.t
val t_of_sexp : t Sexp.Of_sexp.t

(** Whether the parser can read the data or not *)
val can_read : parser_version:t -> data_version:t -> bool
