include String

let default = "default"
let build_dir t = Path.Build.relative Path.Build.root t
let is_default = String.equal default

let of_string_opt name =
  if
    name = ""
    || String.starts_with ~prefix:"." name
    || name = "log"
    || String.contains name '/'
    || String.contains name '\\'
  then None
  else Some name
;;

let target t ~toolchain = Printf.sprintf "%s.%s" (to_string t) (to_string toolchain)
let compare = String.compare

module Infix = Comparator.Operators (String)
module Top_closure = Top_closure.Make (String.Set) (Monad.Id)
