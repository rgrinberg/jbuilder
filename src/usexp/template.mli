type syntax = Dollar_brace | Dollar_paren | Percent

type var =
  { loc: Loc.t
  ; name: string
  ; payload: string
  ; syntax: syntax
  }

type part =
  | Text of string
  | Var of var

type t =
  { quoted: bool
  ; parts: part list
  ; loc: Loc.t
  }

val to_string : t -> string

val sexp_of_t : t -> Sexp.t

val to_debug_sexp : t -> Sexp.t
