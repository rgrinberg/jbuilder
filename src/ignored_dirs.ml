open! Stdune
open Import

let standard =
  let open Re in
  [ empty
  ; str "."
  ; seq [set "._#"; rep any]
  ]
  |> alt
  |> Glob.of_re
  |> Predicate_lang.of_glob

let workspace_defaults (workspace : Workspace.t) =
  List.fold_left ~init:Path.Map.empty workspace.contexts
    ~f:(fun acc (c : Workspace.Context.t) ->
      match Workspace.Context.ignored_subdirs c with
      | None -> acc
      | Some i ->
        let build_dir =
          Path.relative Path.build_dir (Workspace.Context.name c) in
        Path.Map.add acc (Path.root) i)
