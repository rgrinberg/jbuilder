type t =
  { start : Lexing.position
  ; stop  : Lexing.position
  }

val in_file : string -> t
