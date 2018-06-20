open Import

module Version = struct
  type t = int * int

  let to_string (a, b) = sprintf "%u.%u" a b

  let sexp_of_t t = Sexp.unsafe_atom_of_string (to_string t)

  let t : t Sexp.Of_sexp.t =
    let open Sexp.Of_sexp in
    raw >>| function
    | Atom (loc, A s) -> begin
        try
          Scanf.sscanf s "%u.%u" (fun a b -> (a, b))
        with _ ->
          Loc.fail loc "Atom of the form NNN.NNN expected"
      end
    | sexp ->
      of_sexp_error (Sexp.Ast.loc sexp) "Atom expected"

  let can_read ~parser_version:(pa, pb) ~data_version:(da, db) =
    pa = da && db <= pb
end

module Supported_versions = struct
  type t = int Int.Map.t

  let make l : t =
    match
      List.map l ~f:(fun (major, minor) -> (major, minor))
      |> Int.Map.of_list
    with
    | Ok x -> x
    | Error _ ->
      Exn.code_error
        "Syntax.create"
        [ "versions", Sexp.To_sexp.list Version.sexp_of_t l ]

  let greatest_supported_version t = Option.value_exn (Int.Map.max_binding t)

  let is_supported t (major, minor) =
    match Int.Map.find t major with
    | Some minor' -> minor' >= minor
    | None -> false

  let supported_ranges t =
    Int.Map.to_list t |> List.map ~f:(fun (major, minor) ->
      ((major, 0), (major, minor)))
end

type t =
  { name : string
  ; key  : Version.t Univ_map.Key.t
  ; supported_versions : Supported_versions.t
  }

let create ~name supported_versions =
  { name
  ; key = Univ_map.Key.create ()
  ; supported_versions = Supported_versions.make supported_versions
  }

let name t = t.name

let check_supported t (loc, ver) =
  if not (Supported_versions.is_supported t.supported_versions ver) then
    Loc.fail loc "Version %s of %s is not supported.\n\
                  Supported versions:\n\
                  %s"
      (Version.to_string ver) t.name
      (String.concat ~sep:"\n"
         (List.map (Supported_versions.supported_ranges t.supported_versions)
            ~f:(fun (a, b) ->
              sprintf "- %s to %s"
                (Version.to_string a)
                (Version.to_string b))))

let greatest_supported_version t =
  Supported_versions.greatest_supported_version t.supported_versions

let key t = t.key

let set t ver parser =
  Sexp.Of_sexp.set t.key ver parser

let get_exn t =
  let open Sexp.Of_sexp in
  get t.key >>| function
  | Some x -> x
  | None ->
    Exn.code_error "Syntax identifier is unset"
      [ "name", Sexp.To_sexp.string t.name ]
