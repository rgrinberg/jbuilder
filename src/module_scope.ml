type t =
  | Private
  | Public

let to_string = function
  | Public -> "public"
  | Private -> "private"
