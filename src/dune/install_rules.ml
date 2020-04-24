open! Stdune
open Import
open Build.O
open! No_io
module Library = Dune_file.Library

module Package_paths = struct
  let opam_file (ctx : Context.t) pkg =
    Path.Build.append_source ctx.build_dir (Package.opam_file pkg)

  let meta_file (ctx : Context.t) pkg =
    Path.Build.append_source ctx.build_dir (Package.meta_file pkg)

  let deprecated_meta_file (ctx : Context.t) pkg name =
    Path.Build.append_source ctx.build_dir
      (Package.deprecated_meta_file pkg name)

  let build_dir (ctx : Context.t) (pkg : Package.t) =
    Path.Build.append_source ctx.build_dir pkg.path

  let dune_package_file ctx pkg =
    Path.Build.relative (build_dir ctx pkg)
      (Package.Name.to_string pkg.name ^ ".dune-package")

  let deprecated_dune_package_file ctx pkg name =
    Path.Build.relative (build_dir ctx pkg)
      (Package.Name.to_string name ^ ".dune-package")

  let meta_template ctx pkg =
    Path.Build.extend_basename (meta_file ctx pkg) ~suffix:".template"
end

module Stanzas_to_entries : sig
  val stanzas_to_entries :
       Super_context.t
    -> (Loc.t option * Path.Build.t Install.Entry.t) list Package.Name.Map.t
end = struct
  let lib_ppxs sctx ~scope ~(lib : Dune_file.Library.t) =
    match lib.kind with
    | Normal
    | Ppx_deriver _ ->
      []
    | Ppx_rewriter _ ->
      let name = Dune_file.Library.best_name lib in
      [ Preprocessing.ppx_exe sctx ~scope name |> Result.ok_exn ]

  let lib_install_files sctx ~scope ~dir_contents ~dir ~sub_dir:lib_subdir
      (lib : Library.t) =
    let loc = lib.buildable.loc in
    let obj_dir = Dune_file.Library.obj_dir lib ~dir in
    let make_entry section ?sub_dir ?dst fn =
      ( Some loc
      , Install.Entry.make section fn
          ~dst:
            (let dst =
               match dst with
               | Some s -> s
               | None -> Path.Build.basename fn
             in
             let sub_dir =
               match sub_dir with
               | Some _ -> sub_dir
               | None -> lib_subdir
             in
             match sub_dir with
             | None -> dst
             | Some dir -> sprintf "%s/%s" dir dst) )
    in
    let installable_modules =
      Dir_contents.ocaml dir_contents
      |> Ml_sources.modules_of_library ~name:(Library.best_name lib)
      |> Modules.fold_no_vlib ~init:[] ~f:(fun m acc -> m :: acc)
    in
    let sources =
      List.concat_map installable_modules ~f:(fun m ->
          List.map (Module.sources m) ~f:(fun source ->
              (* We add the -gen suffix to a few files generated by dune, such
                 as the alias module. *)
              let source = Path.as_in_build_dir_exn source in
              let dst =
                Path.Build.basename source |> String.drop_suffix ~suffix:"-gen"
              in
              make_entry Lib source ?dst))
    in
    let ctx = Super_context.context sctx in
    let module_files =
      let { Lib_config.has_native; ext_obj; _ } = ctx.lib_config in
      let if_ cond l =
        if cond then
          l
        else
          []
      in
      let { Mode.Dict.byte; native } =
        Dune_file.Mode_conf.Set.eval lib.modes ~has_native
      in
      let virtual_library = Library.is_virtual lib in
      List.concat_map installable_modules ~f:(fun m ->
          let cm_file_unsafe kind =
            Obj_dir.Module.cm_file_unsafe obj_dir m ~kind
          in
          let cmi_file = (Module.visibility m, cm_file_unsafe Cmi) in
          let other_cm_files =
            let has_impl = Module.has ~ml_kind:Impl m in
            [ if_ (native && has_impl) [ cm_file_unsafe Cmx ]
            ; if_ (byte && has_impl && virtual_library) [ cm_file_unsafe Cmo ]
            ; if_
                (native && has_impl && virtual_library)
                [ Obj_dir.Module.obj_file obj_dir m ~kind:Cmx ~ext:ext_obj ]
            ; List.filter_map Ml_kind.all ~f:(fun ml_kind ->
                  Obj_dir.Module.cmt_file obj_dir m ~ml_kind)
            ]
            |> List.concat
            |> List.map ~f:(fun f -> (Visibility.Public, f))
          in
          cmi_file :: other_cm_files)
    in
    let archives = Lib_archives.make ~ctx ~dir_contents ~dir lib in
    let execs = lib_ppxs sctx ~scope ~lib in
    List.concat
      [ sources
      ; List.map module_files ~f:(fun (visibility, file) ->
            let sub_dir =
              match ((visibility : Visibility.t), lib_subdir) with
              | Public, _ -> lib_subdir
              | Private, None -> Some ".private"
              | Private, Some dir -> Some (Filename.concat dir ".private")
            in
            make_entry ?sub_dir Lib file)
      ; List.map (Lib_archives.lib_files archives) ~f:(make_entry Lib)
      ; List.map execs ~f:(make_entry Libexec)
      ; List.map (Lib_archives.dll_files archives) ~f:(fun a ->
            (Some loc, Install.Entry.make Stublibs a))
      ]

  let keep_if ~external_lib_deps_mode expander =
    if external_lib_deps_mode then
      fun ~scope:_ ->
    Option.some
    else
      fun ~scope stanza ->
    Option.some_if
      ( match (stanza : Stanza.t) with
      | Dune_file.Library lib ->
        (not lib.optional)
        || Lib.DB.available (Scope.libs scope) (Dune_file.Library.best_name lib)
      | Dune_file.Documentation _
      | Dune_file.Install _ ->
        true
      | Dune_file.Executables ({ install_conf = Some _; _ } as exes) ->
        Expander.eval_blang expander exes.enabled_if
        && ( (not exes.optional)
           ||
           let compile_info =
             let dune_version =
               Scope.project scope |> Dune_project.dune_version
             in
             Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope)
               exes.names exes.buildable.libraries
               ~pps:(Dune_file.Preprocess_map.pps exes.buildable.preprocess)
               ~dune_version
               ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
               ~variants:exes.variants ~optional:exes.optional
           in
           Result.is_ok (Lib.Compile.direct_requires compile_info) )
      | Coq_stanza.Theory.T d -> Option.is_some d.package
      | _ -> false )
      stanza

  let is_odig_doc_file fn =
    List.exists [ "README"; "LICENSE"; "CHANGE"; "HISTORY" ] ~f:(fun prefix ->
        String.is_prefix fn ~prefix)

  let stanzas_to_entries sctx =
    let ctx = Super_context.context sctx in
    let stanzas = Super_context.stanzas sctx in
    let init =
      Super_context.packages sctx
      |> Package.Name.Map.map ~f:(fun (pkg : Package.t) ->
             let init =
               let deprecated_meta_and_dune_files =
                 List.concat_map
                   (Package.Name.Map.to_list pkg.deprecated_package_names)
                   ~f:(fun (name, _) ->
                     let meta_file =
                       Package_paths.deprecated_meta_file ctx pkg name
                     in
                     let dune_package_file =
                       Package_paths.deprecated_dune_package_file ctx pkg name
                     in
                     [ ( None
                       , Install.Entry.make Lib_root meta_file
                           ~dst:
                             ( Package.Name.to_string name
                             ^ "/" ^ Findlib.meta_fn ) )
                     ; ( None
                       , Install.Entry.make Lib_root dune_package_file
                           ~dst:
                             ( Package.Name.to_string name
                             ^ "/" ^ Dune_package.fn ) )
                     ])
               in
               let meta_file = Package_paths.meta_file ctx pkg in
               let dune_package_file =
                 Package_paths.dune_package_file ctx pkg
               in
               (None, Install.Entry.make Lib meta_file ~dst:Findlib.meta_fn)
               :: ( None
                  , Install.Entry.make Lib dune_package_file
                      ~dst:Dune_package.fn )
               ::
               ( if not pkg.has_opam_file then
                 deprecated_meta_and_dune_files
               else
                 let opam_file = Package_paths.opam_file ctx pkg in
                 (None, Install.Entry.make Lib opam_file ~dst:"opam")
                 :: deprecated_meta_and_dune_files )
             in
             match File_tree.find_dir pkg.path with
             | None -> init
             | Some dir ->
               let pkg_dir = Path.Build.append_source ctx.build_dir pkg.path in
               File_tree.Dir.files dir
               |> String.Set.fold ~init ~f:(fun fn acc ->
                      if is_odig_doc_file fn then
                        let odig_file = Path.Build.relative pkg_dir fn in
                        let entry = (None, Install.Entry.make Doc odig_file) in
                        entry :: acc
                      else
                        acc))
    in
    let keep_if =
      let external_lib_deps_mode = !Clflags.external_lib_deps_mode in
      keep_if ~external_lib_deps_mode
    in
    Dir_with_dune.deep_fold stanzas ~init ~f:(fun d stanza acc ->
        let { Dir_with_dune.ctx_dir = dir; scope; _ } = d in
        let expander = Super_context.expander sctx ~dir in
        let res =
          let open Option.O in
          let* stanza = keep_if expander stanza ~scope in
          let+ package = Dune_file.stanza_package stanza in
          (stanza, package)
        in
        match res with
        | None -> acc
        | Some (stanza, package) ->
          let new_entries =
            match (stanza : Stanza.t) with
            | Dune_file.Install i
            | Dune_file.Executables { install_conf = Some i; _ } ->
              let path_expander =
                File_binding.Unexpanded.expand ~dir
                  ~f:(Expander.expand_str expander)
              in
              let section = i.section in
              List.map i.files ~f:(fun unexpanded ->
                  let fb = path_expander unexpanded in
                  let loc = File_binding.Expanded.src_loc fb in
                  let src = File_binding.Expanded.src fb in
                  let dst = File_binding.Expanded.dst fb in
                  (Some loc, Install.Entry.make section src ?dst))
            | Dune_file.Library lib ->
              let sub_dir = (Option.value_exn lib.public).sub_dir in
              let dir_contents = Dir_contents.get sctx ~dir in
              lib_install_files sctx ~scope ~dir ~sub_dir lib ~dir_contents
            | Coq_stanza.Theory.T coqlib ->
              Coq_rules.install_rules ~sctx ~dir coqlib
            | Dune_file.Documentation d ->
              let dc = Dir_contents.get sctx ~dir in
              let mlds = Dir_contents.mlds dc d in
              List.map mlds ~f:(fun mld ->
                  ( None
                  , Install.Entry.make
                      ~dst:(sprintf "odoc-pages/%s" (Path.Build.basename mld))
                      Install.Section.Doc mld ))
            | _ -> []
          in
          Package.Name.Map.Multi.add_all acc package.name new_entries)

  let stanzas_to_entries =
    let memo =
      Memo.create
        ~input:(module Super_context.As_memo_key)
        ~output:
          (Simple
             ( module struct
               type t =
                 (Loc.t option * Path.Build.t Install.Entry.t) list
                 Package.Name.Map.t

               let to_dyn _ = Dyn.Opaque
             end ))
        "stanzas-to-entries" ~doc:"install entries for all packages"
        ~visibility:Hidden Sync stanzas_to_entries
    in
    Memo.exec memo
end

module Meta_and_dune_package : sig
  val meta_and_dune_package_rules : Super_context.t -> dir:Path.Build.t -> unit
end = struct
  let gen_dune_package sctx (pkg : Package.t) =
    let ctx = Super_context.context sctx in
    let name = pkg.name in
    let dune_version =
      Option.value_exn
        (Dune_lang.Syntax.greatest_supported_version Stanza.syntax)
    in
    let lib_entries = Super_context.lib_entries_of_package sctx pkg.name in
    let action =
      let gen_dune_package () =
        let dune_package =
          let pkg_root =
            Config.local_install_lib_dir ~context:ctx.name ~package:name
          in
          let lib_root lib =
            let _, subdir = Lib_name.split (Lib.name lib) in
            Path.Build.L.relative pkg_root subdir
          in
          let entries =
            List.fold_left lib_entries ~init:Lib_name.Map.empty
              ~f:(fun acc stanza ->
                match stanza with
                | Super_context.Lib_entry.Deprecated_library_name
                    { old_public_name = { deprecated = true; _ }; _ } ->
                  acc
                | Super_context.Lib_entry.Deprecated_library_name
                    { old_public_name =
                        { public = old_public_name; deprecated = false }
                    ; new_public_name = _, new_public_name
                    ; loc
                    ; _
                    } ->
                  let old_public_name =
                    Dune_file.Public_lib.name old_public_name
                  in
                  Lib_name.Map.add_exn acc old_public_name
                    (Dune_package.Entry.Deprecated_library_name
                       { loc; old_public_name; new_public_name })
                | Library lib ->
                  let dir_contents =
                    let info = Lib.Local.info lib in
                    let dir = Lib_info.src_dir info in
                    Dir_contents.get sctx ~dir
                  in
                  let obj_dir = Lib.Local.obj_dir lib in
                  let lib = Lib.Local.to_lib lib in
                  let name = Lib.name lib in
                  let foreign_objects =
                    (* We are writing the list of .o files to dune-package, but
                       we actually only install them for virtual libraries. See
                       [Lib_archives.make] *)
                    let dir = Obj_dir.obj_dir obj_dir in
                    Dir_contents.foreign_sources dir_contents
                    |> Foreign_sources.for_lib ~name
                    |> Foreign.Sources.object_files ~dir
                         ~ext_obj:ctx.lib_config.ext_obj
                    |> List.map ~f:Path.build
                  in
                  let modules =
                    Dir_contents.ocaml dir_contents
                    |> Ml_sources.modules_of_library ~name
                  in
                  Lib_name.Map.add_exn acc name
                    (Library
                       (Result.ok_exn
                          (Lib.to_dune_lib lib
                             ~dir:(Path.build (lib_root lib))
                             ~modules ~foreign_objects))))
          in
          Dune_package.Or_meta.Dune_package
            { Dune_package.version = pkg.version
            ; name
            ; entries
            ; dir = Path.build pkg_root
            }
        in
        dune_package
      in
      let dune_package_file = Package_paths.dune_package_file ctx pkg in
      let meta_template = Package_paths.meta_template ctx pkg in
      Build.write_file_dyn dune_package_file
        (let+ pkg =
           Build.if_file_exists (Path.build meta_template)
             ~then_:(Build.return Dune_package.Or_meta.Use_meta)
             ~else_:(Build.delayed gen_dune_package)
         in
         Format.asprintf "%a" (Dune_package.Or_meta.pp ~dune_version) pkg)
    in
    let deprecated_dune_packages =
      List.filter_map lib_entries ~f:(function
        | Super_context.Lib_entry.Deprecated_library_name
            ( { old_public_name = { deprecated = true; public = old_public_name }
              ; _
              } as t ) ->
          Some
            ( Lib_name.package_name (Dune_file.Public_lib.name old_public_name)
            , t )
        | _ -> None)
      |> Package.Name.Map.of_list_multi
    in
    Package.Name.Map.iteri pkg.deprecated_package_names ~f:(fun name _ ->
        let dune_pkg =
          let entries =
            match Package.Name.Map.find deprecated_dune_packages name with
            | None -> Lib_name.Map.empty
            | Some entries ->
              List.fold_left entries ~init:Lib_name.Map.empty
                ~f:(fun acc
                        { Dune_file.Deprecated_library_name.old_public_name =
                            { public = old_public_name; _ }
                        ; new_public_name = _, new_public_name
                        ; loc
                        ; _
                        }
                        ->
                  let old_public_name =
                    Dune_file.Public_lib.name old_public_name
                  in
                  Lib_name.Map.add_exn acc old_public_name
                    (Dune_package.Entry.Deprecated_library_name
                       { loc; old_public_name; new_public_name }))
          in
          { Dune_package.version = pkg.version
          ; name
          ; entries
          ; dir =
              Path.build
                (Config.local_install_lib_dir ~context:ctx.name ~package:name)
          }
        in
        Build.write_file
          (Package_paths.deprecated_dune_package_file ctx pkg
             dune_pkg.Dune_package.name)
          (Format.asprintf "%a"
             (Dune_package.Or_meta.pp ~dune_version)
             (Dune_package.Or_meta.Dune_package dune_pkg))
        |> Super_context.add_rule sctx ~dir:ctx.build_dir);
    Super_context.add_rule sctx ~dir:ctx.build_dir action

  let gen_meta_file sctx (pkg : Package.t) =
    let ctx = Super_context.context sctx in
    let deprecated_packages, entries =
      let entries = Super_context.lib_entries_of_package sctx pkg.name in
      List.partition_map entries ~f:(function
        | Super_context.Lib_entry.Deprecated_library_name
            { old_public_name =
                { deprecated = true
                ; public = { sub_dir = None; name = _, name; _ }
                }
            ; _
            } as entry ->
          Left (Lib_name.package_name name, entry)
        | entry -> Right entry)
    in
    let template =
      let meta_template = Path.build (Package_paths.meta_template ctx pkg) in
      let meta_template_lines_or_fail =
        (* XXX this should really be lazy as it's only necessary for the then
           clause. There's no way to express this in the build description
           however. *)
        let vlib =
          List.find_map entries ~f:(function
            | Super_context.Lib_entry.Library lib ->
              let info = Lib.Local.info lib in
              Option.some_if (Option.is_some (Lib_info.virtual_ info)) lib
            | Deprecated_library_name _ -> None)
        in
        match vlib with
        | None -> Build.lines_of meta_template
        | Some vlib ->
          Build.fail
            { fail =
                (fun () ->
                  let name = Lib.name (Lib.Local.to_lib vlib) in
                  User_error.raise
                    ~loc:(Loc.in_file meta_template)
                    [ Pp.textf
                        "Package %s defines virtual library %s and has a META \
                         template. This is not allowed."
                        (Package.Name.to_string pkg.name)
                        (Lib_name.to_string name)
                    ])
            }
      in
      Build.if_file_exists meta_template ~then_:meta_template_lines_or_fail
        ~else_:(Build.return [ "# DUNE_GEN" ])
    in
    let ctx = Super_context.context sctx in
    let meta = Package_paths.meta_file ctx pkg in
    Super_context.add_rule sctx ~dir:ctx.build_dir
      (let open Build.O in
      (let+ template = template in
       let meta =
         Gen_meta.gen
           ~package:(Package.Name.to_string pkg.name)
           ~version:pkg.version entries
       in
       let pp =
         Pp.vbox
           (Pp.concat_map template ~sep:Pp.newline ~f:(fun s ->
                if String.is_prefix s ~prefix:"#" then
                  match
                    String.extract_blank_separated_words (String.drop s 1)
                  with
                  | [ ("JBUILDER_GEN" | "DUNE_GEN") ] -> Meta.pp meta.entries
                  | _ -> Pp.verbatim s
                else
                  Pp.verbatim s))
       in
       Format.asprintf "%a" Pp.render_ignore_tags pp)
      |> Build.write_file_dyn meta);
    let deprecated_packages =
      Package.Name.Map.of_list_multi deprecated_packages
    in
    Package.Name.Map.iteri pkg.deprecated_package_names ~f:(fun name _ ->
        let meta = Package_paths.deprecated_meta_file ctx pkg name in
        Super_context.add_rule sctx ~dir:ctx.build_dir
          ( (let meta =
               let entries =
                 match Package.Name.Map.find deprecated_packages name with
                 | None -> []
                 | Some entries -> entries
               in
               Gen_meta.gen
                 ~package:(Package.Name.to_string pkg.name)
                 ~version:pkg.version entries ~add_directory_entry:false
             in
             let pp =
               let open Pp.O in
               Pp.vbox (Meta.pp meta.entries ++ Pp.cut)
             in
             Format.asprintf "%a" Pp.render_ignore_tags pp)
          |> Build.write_file meta ))

  let meta_and_dune_package_rules_impl (project, sctx) =
    Dune_project.packages project
    |> Package.Name.Map.iter ~f:(fun (pkg : Package.t) ->
           gen_dune_package sctx pkg;
           gen_meta_file sctx pkg)

  let meta_and_dune_package_rules_memo =
    let module Project_and_super_context = struct
      type t = Dune_project.t * Super_context.t

      let equal =
        Tuple.T2.equal Dune_project.equal Super_context.As_memo_key.equal

      let hash = Tuple.T2.hash Dune_project.hash Super_context.As_memo_key.hash

      let to_dyn (p, s) =
        Dyn.Tuple [ Dune_project.to_dyn p; Super_context.As_memo_key.to_dyn s ]
    end in
    Memo.With_implicit_output.create "meta_and_dune_package_rules"
      ~input:(module Project_and_super_context)
      ~visibility:Hidden
      ~output:(module Unit)
      ~implicit_output:Rules.implicit_output Sync
      meta_and_dune_package_rules_impl

  let meta_and_dune_package_rules sctx ~dir =
    let project = Scope.project (Super_context.find_scope_by_dir sctx dir) in
    Memo.With_implicit_output.exec meta_and_dune_package_rules_memo
      (project, sctx)
end

include Meta_and_dune_package

let symlink_installed_artifacts_to_build_install sctx
    (entries : (Loc.t option * Path.Build.t Install.Entry.t) list)
    ~install_paths =
  let ctx = Super_context.context sctx in
  let install_dir = Config.local_install_dir ~context:ctx.name in
  List.map entries ~f:(fun (loc, entry) ->
      let dst =
        let relative =
          Install.Entry.relative_installed_path entry ~paths:install_paths
          |> Path.as_in_source_tree_exn
        in
        Path.append_source (Path.build install_dir) relative
        |> Path.as_in_build_dir_exn
      in
      let loc =
        match loc with
        | Some l -> l
        | None -> Loc.in_file (Path.build entry.src)
      in
      Super_context.add_rule sctx ~loc ~dir:ctx.build_dir
        (Build.symlink ~src:(Path.build entry.src) ~dst);
      Install.Entry.set_src entry dst)

let promote_install_file (ctx : Context.t) =
  !Clflags.promote_install_files
  && (not ctx.implicit)
  &&
  match ctx.kind with
  | Default -> true
  | Opam _ -> false

let install_entries sctx (package : Package.t) =
  let packages = Stanzas_to_entries.stanzas_to_entries sctx in
  Package.Name.Map.Multi.find packages package.name

let install_rules sctx (package : Package.t) =
  let install_paths =
    Install.Section.Paths.make ~package:package.name ~destdir:Path.root ()
  in
  let entries =
    install_entries sctx package
    |> symlink_installed_artifacts_to_build_install sctx ~install_paths
  in
  let ctx = Super_context.context sctx in
  let pkg_build_dir = Package_paths.build_dir ctx package in
  let files = Install.files entries in
  let strict_package_deps =
    let scope = Super_context.find_scope_by_dir sctx pkg_build_dir in
    let dune_project = Scope.project scope in
    Dune_project.strict_package_deps dune_project
  in
  let packages =
    let+ packages = Build_system.package_deps package.name files in
    match strict_package_deps with
    | false -> packages
    | true ->
      let missing_deps =
        Package.missing_deps package ~effective_deps:packages
      in
      if Package.Name.Set.is_empty missing_deps then
        packages
      else
        User_error.raise
          [ Pp.textf "Package %s is missing the following package dependencies"
              (Package.Name.to_string package.name)
          ; Package.Name.Set.to_list missing_deps
            |> Pp.enumerate ~f:(fun name ->
                   Pp.text (Package.Name.to_string name))
          ]
  in
  let () =
    let target_alias =
      Build_system.Alias.package_install ~context:ctx ~pkg:package
    in
    Rules.Produce.Alias.add_deps target_alias files
      ~dyn_deps:
        (let+ packages = packages in
         Package.Name.Set.to_list packages
         |> Path.Set.of_list_map ~f:(fun pkg ->
                let pkg =
                  Package.Name.Map.find_exn (Super_context.packages sctx) pkg
                in
                Build_system.Alias.package_install ~context:ctx ~pkg
                |> Alias.stamp_file |> Path.build))
  in
  let action =
    let install_file =
      Path.Build.relative pkg_build_dir
        (Utils.install_file ~package:package.name
           ~findlib_toolchain:ctx.findlib_toolchain)
    in
    Build.write_file_dyn install_file
      (let+ () = Build.path_set files
       and+ () =
         if strict_package_deps then
           Build.map packages ~f:(fun (_ : Package.Name.Set.t) -> ())
         else
           Build.return ()
       in
       let entries =
         match ctx.findlib_toolchain with
         | None -> entries
         | Some toolchain ->
           let toolchain = Context_name.to_string toolchain in
           let prefix = Path.of_string (toolchain ^ "-sysroot") in
           List.map entries
             ~f:(Install.Entry.add_install_prefix ~paths:install_paths ~prefix)
       in
       Install.gen_install_file entries)
  in
  Super_context.add_rule sctx ~dir:pkg_build_dir
    ~mode:
      ( if promote_install_file ctx then
        Promote { lifetime = Until_clean; into = None; only = None }
      else
        (* We must ignore the source file since it might be copied to the source
           tree by another context. *)
        Ignore_source_files )
    action

let memo =
  let module Scheme = struct
    type t = Rules.Dir_rules.t Scheme.t

    let to_dyn _ = Dyn.Opaque
  end in
  let module Sctx_and_package = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Package.t

    let hash (x, y) = Hashtbl.hash (Super_context.hash x, Package.hash y)

    let equal (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

    let to_dyn _ = Dyn.Opaque
  end in
  let install_alias (ctx : Context.t) (package : Package.t) =
    if not ctx.implicit then
      let install_fn =
        Utils.install_file ~package:package.name
          ~findlib_toolchain:ctx.findlib_toolchain
      in
      let path = Package_paths.build_dir ctx package in
      let install_alias = Alias.install ~dir:path in
      let install_file = Path.relative (Path.build path) install_fn in
      Rules.Produce.Alias.add_deps install_alias
        (Path.Set.singleton install_file)
  in
  Memo.create
    ~input:(module Sctx_and_package)
    ~output:(Simple (module Scheme))
    "install-rules-and-pkg-entries" ~doc:"install rules and package entries"
    ~visibility:Hidden Sync
    (fun (sctx, pkg) ->
      let ctx = Super_context.context sctx in
      let context_name = ctx.name in
      let rules =
        Memo.lazy_ ~loc:(Loc.of_pos __POS__) (fun () ->
            Rules.collect_unit (fun () ->
                install_rules sctx pkg;
                install_alias ctx pkg))
      in
      Approximation
        ( Dir_set.union_all
            [ Dir_set.subtree (Config.local_install_dir ~context:context_name)
            ; Dir_set.singleton (Package_paths.build_dir ctx pkg)
            ]
        , Thunk (fun () -> Finite (Rules.to_map (Memo.Lazy.force rules))) ))

let scheme sctx pkg = Memo.exec memo (sctx, pkg)

let scheme_per_ctx_memo =
  Memo.create
    ~input:(module Super_context.As_memo_key)
    ~output:
      (Simple
         ( module struct
           type t = Rules.Dir_rules.t Scheme.Evaluated.t

           let to_dyn _ = Dyn.Opaque
         end ))
    "install-rule-scheme" ~doc:"install rules scheme" ~visibility:Hidden Sync
    (fun sctx ->
      let packages = Super_context.packages sctx in
      let scheme =
        Scheme.all
          (List.map (Package.Name.Map.to_list packages) ~f:(fun (_, pkg) ->
               scheme sctx pkg))
      in
      Scheme.evaluate ~union:Rules.Dir_rules.union scheme)

let gen_rules sctx ~dir =
  let rules, subdirs =
    Scheme.Evaluated.get_rules (Memo.exec scheme_per_ctx_memo sctx) ~dir
  in
  Rules.produce_dir ~dir (Option.value ~default:Rules.Dir_rules.empty rules);
  Build_system.Subdir_set.These subdirs

let packages =
  let package_source_files sctx package =
    List.map
      ~f:(fun (_loc, entry) -> entry.Install.Entry.src)
      (install_entries sctx package)
  in
  let f sctx =
    Super_context.packages sctx
    |> Package.Name.Map.foldi ~init:[] ~f:(fun name pkg acc ->
           List.fold_left (package_source_files sctx pkg) ~init:acc
             ~f:(fun acc path -> (path, name) :: acc))
    |> Path.Build.Map.of_list_fold ~init:Package.Name.Set.empty
         ~f:Package.Name.Set.add
  in
  let memo =
    Memo.create "package-map" ~doc:"Return a map assining package to files"
      ~input:(module Super_context.As_memo_key)
      ~visibility:Hidden
      ~output:
        (Allow_cutoff
           ( module struct
             type t = Package.Name.Set.t Path.Build.Map.t

             let to_dyn = Path.Build.Map.to_dyn Package.Name.Set.to_dyn

             let equal = Path.Build.Map.equal ~equal:Package.Name.Set.equal
           end ))
      Sync f
  in
  fun sctx -> Memo.exec memo sctx
