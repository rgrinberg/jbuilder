(** Result type for OCaml < 4.03 *)

type ('a, 'error) t =
  | Ok    of 'a
  | Error of 'error
