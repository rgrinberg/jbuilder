open Import

include Interned.Make()

let ppx_driver = make "ppx_driver"
let mt         = make "mt"
let mt_posix   = make "mt_posix"
let byte       = make "byte"
let native     = make "native"
let plugin     = make "plugin"

module Rules = struct
  type 'a t = 'a Map.t

  let get t ~variants =
    Set.to_list variants
    |> List.map ~f:(fun k -> Option.value_exn (Map.find t k))

  let map t ~f = Map.map t ~f

  let make l = Map.of_list_exn l

  let of_meta_rules (s : Meta.Simplified.Rules.t) : string t =
    ignore s;
    Map.empty
end
