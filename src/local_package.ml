open Stdune

type t =
  { odig_files : Path.t list
  ; ctx_build_dir : Path.t
  ; lib_stanzas : Dune_file.Library.t Dir_with_dune.t list
  ; installs : string Dune_file.Install_conf.t Dir_with_dune.t list
  ; docs : Dune_file.Documentation.t list
  ; mlds : Path.t list
  ; pkg : Package.t
  ; libs : Lib.Set.t
  ; sctx : Super_context.t
  }

let is_odig_doc_file fn =
  List.exists [ "README"; "LICENSE"; "CHANGE"; "HISTORY"]
    ~f:(fun prefix -> String.is_prefix fn ~prefix)

let add_stanzas t ~sctx =
  List.fold_left ~init:t
    ~f:(fun t ({ Dir_with_dune. ctx_dir = dir ; scope ; data
               ; src_dir = _ ; kind = _} as d) ->
      let path_expander = Super_context.expand_vars_string sctx ~scope ~dir in
      let open Dune_file in
      match data with
      | Install i ->
        let i = { i with files = File_bindings.map ~f:path_expander i.files } in
        { t with
          installs = { d with data = i } :: t.installs
        }
      | Library l ->
        { t with
          lib_stanzas = { d with data = l } :: t.lib_stanzas
        }
      | Documentation l ->
        { t with
          docs = l :: t.docs
        ; mlds =
            let dir_contents = Dir_contents.get sctx ~dir in
            List.rev_append (Dir_contents.mlds dir_contents l)
              t.mlds
        }
      | _ -> t)

let stanzas_to_consider_for_install stanzas ~external_lib_deps_mode =
  if not external_lib_deps_mode then
    List.concat_map stanzas
      ~f:(fun ({ Dir_with_dune.ctx_dir =  _; data = stanzas
               ; scope; kind = _ ; src_dir = _ } as d) ->
           List.filter_map stanzas ~f:(fun stanza ->
             let keep =
               match (stanza : Stanza.t) with
               | Dune_file.Library lib ->
                 Lib.DB.available (Scope.libs scope)
                   (Dune_file.Library.best_name lib)
               | Dune_file.Documentation _
               | Dune_file.Install _ -> true
               | _ -> false
             in
             Option.some_if keep { d with data = stanza }))
  else
    List.concat_map stanzas
      ~f:(fun d ->
        List.map d.data ~f:(fun stanza ->
          { d with data = stanza}))

let of_sctx (sctx : Super_context.t) =
  let ctx = Super_context.context sctx in
  let stanzas =
    let stanzas = Super_context.stanzas sctx in
    let external_lib_deps_mode =
      Super_context.external_lib_deps_mode sctx in
    stanzas_to_consider_for_install stanzas ~external_lib_deps_mode
  in
  let stanzas_per_package =
    List.filter_map stanzas
      ~f:(fun (installable : Stanza.t Dir_with_dune.t) ->
        match Dune_file.stanza_package installable.data with
        | None -> None
        | Some p -> Some (p.name, installable))
    |> Package.Name.Map.of_list_multi
  in
  let libs_of =
    let libs = Super_context.libs_by_package sctx in
    fun (pkg : Package.t) ->
      match Package.Name.Map.find libs pkg.name with
      | Some (_, libs) -> libs
      | None -> Lib.Set.empty
  in
  Super_context.packages sctx
  |> Package.Name.Map.map ~f:(fun (pkg : Package.t) ->
    let odig_files =
      let files = Super_context.source_files sctx ~src_path:Path.root in
      String.Set.fold files ~init:[] ~f:(fun fn acc ->
        if is_odig_doc_file fn then
          Path.relative ctx.build_dir fn :: acc
        else
          acc)
    in
    let libs = libs_of pkg in
    let t =
      add_stanzas
        ~sctx
        { odig_files
        ; lib_stanzas = []
        ; docs = []
        ; installs = []
        ; pkg
        ; ctx_build_dir = ctx.build_dir
        ; libs
        ; mlds = []
        ; sctx
        }
        (Package.Name.Map.find stanzas_per_package pkg.name
         |> Option.value ~default:[])
    in
    t
  )

let odig_files t = t.odig_files
let libs t = t.libs
let docs t = t.docs
let installs t = t.installs
let lib_stanzas t = t.lib_stanzas
let mlds t = t.mlds

let package t = t.pkg
let opam_file t = Path.append t.ctx_build_dir (Package.opam_file t.pkg)
let meta_file t = Path.append t.ctx_build_dir (Package.meta_file t.pkg)
let build_dir t = Path.append t.ctx_build_dir t.pkg.path
let name t = t.pkg.name

let install_paths t =
  Install.Section.Paths.make ~package:t.pkg.name ~destdir:Path.root ()

let project t =
  let dir = build_dir t in
  Scope.project (Super_context.find_scope_by_dir t.sctx dir)

module Version = struct
  open Build.O

  module V = Vfile_kind.Make(struct
      type t = string option
      let encode = Dune_lang.Encoder.(option string)
      let name = "version"
    end)

  let spec t =
    let fn =
      Path.relative (build_dir t)
        (sprintf "%s.version.sexp" (Package.Name.to_string (name t)))
    in
    Build.Vspec.T (fn, (module V))

  let read t = Build.vpath (spec t)

  let set t get =
    let spec = spec t in
    Super_context.add_rule t.sctx ~dir:(build_dir t)
      (get >>> Build.store_vfile spec);
    Build.vpath spec

  type version_method =
    | File of string
    | From_dune_project

  let version t =
    match t.pkg.version_from_opam_file with
    | Some s -> Build.return (Some s)
    | None ->
      let rec loop = function
        | [] -> Build.return None
        | candidate :: rest ->
          match candidate with
          | File fn ->
            let p = Path.relative (build_dir t) fn in
            Build.if_file_exists p
              ~then_:(Build.lines_of p
                      >>^ function
                      | ver :: _ -> Some ver
                      | _ -> Some "")
              ~else_:(loop rest)
          | From_dune_project ->
            match Dune_project.version (project t) with
            | None -> loop rest
            | Some _ as x -> Build.return x
      in
      loop
        [ File (Package.Name.version_fn (name t))
        ; From_dune_project
        ; File "version"
        ; File "VERSION"
        ]
end

let version = Version.version
