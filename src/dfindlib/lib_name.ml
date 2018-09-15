open Stdune

type t = string

let to_string s = s

let of_string_exn ~loc:_ s = s

module Map = Map.Make(String)
module Set = struct
  include Set.Make(String)
  let to_string_list = to_list
end

let compare = String.compare

let root_lib t =
  match String.lsplit2 t ~on:'.' with
  | None -> t
  | Some (p, _) -> p

let pp = Format.pp_print_string

let pp_quoted fmt t = Format.fprintf fmt "%S" t

let nest x y = sprintf "%s.%s" x y
