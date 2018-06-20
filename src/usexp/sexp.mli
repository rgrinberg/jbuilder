type t =
  | Atom of Atom.t
  | Quoted_string of string
  | List of t list

val atom_or_quoted_string : string -> t
