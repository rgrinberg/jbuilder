open! Dune_engine
open! Stdune
open Import
open Memo.Build.O
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
    let dir = Package.dir pkg in
    Path.Build.append_source ctx.build_dir dir

  let dune_package_file ctx pkg =
    let name = Package.name pkg in
    Path.Build.relative (build_dir ctx pkg)
      (Package.Name.to_string name ^ ".dune-package")

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
       Memo.Build.t
end = struct
  let lib_ppxs sctx ~scope ~(lib : Dune_file.Library.t) =
    match lib.kind with
    | Normal
    | Ppx_deriver _ ->
      Memo.Build.return []
    | Ppx_rewriter _ ->
      let name = Dune_file.Library.best_name lib in
      let+ ppx_exe =
        Resolve.Build.read_memo_build (Preprocessing.ppx_exe sctx ~scope name)
      in
      [ ppx_exe ]

  let if_ cond l =
    if cond then
      l
    else
      []

  let lib_files ~dir_contents ~dir ~lib_config lib =
    let virtual_library = Option.is_some (Lib_info.virtual_ lib) in
    let { Lib_config.ext_obj; _ } = lib_config in
    let archives = Lib_info.archives lib in
    let+ modules =
      let+ ml_sources = Dir_contents.ocaml dir_contents in
      Some (Ml_sources.modules ml_sources ~for_:(Library (Lib_info.name lib)))
    and+ foreign_archives =
      if virtual_library then
        let+ foreign_sources = Dir_contents.foreign_sources dir_contents in
        let name = Lib_info.name lib in
        let files = Foreign_sources.for_lib foreign_sources ~name in
        Foreign.Sources.object_files files ~dir ~ext_obj
      else
        Memo.Build.return (Lib_info.foreign_archives lib)
    in
    List.concat_map
      ~f:(List.map ~f:(fun f -> (Section.Lib, f)))
      [ archives.byte
      ; archives.native
      ; foreign_archives
      ; Lib_info.eval_native_archives_exn lib ~modules
      ; Lib_info.jsoo_runtime lib
      ]
    @ List.map ~f:(fun f -> (Section.Libexec, f)) (Lib_info.plugins lib).native

  let dll_files ~(modes : Mode.Dict.Set.t) ~dynlink ~(ctx : Context.t) lib =
    if_
      (modes.byte
      && Dynlink_supported.get dynlink ctx.supports_shared_libraries
      && ctx.dynamically_linked_foreign_archives)
      (Lib_info.foreign_dll_files lib)

  let lib_install_files sctx ~scope ~dir_contents ~dir ~sub_dir:lib_subdir
      (lib : Library.t) =
    let loc = lib.buildable.loc in
    let ctx = Super_context.context sctx in
    let lib_config = ctx.lib_config in
    let* info = Dune_file.Library.to_lib_info lib ~dir ~lib_config in
    let obj_dir = Lib_info.obj_dir info in
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
    let* installable_modules =
      let+ ml_sources = Dir_contents.ocaml dir_contents in
      Ml_sources.modules ml_sources ~for_:(Library (Library.best_name lib))
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
    let { Lib_config.has_native; ext_obj; _ } = lib_config in
    let modes = Dune_file.Mode_conf.Set.eval lib.modes ~has_native in
    let { Mode.Dict.byte; native } = modes in
    let module_files =
      let inside_subdir f =
        match lib_subdir with
        | None -> f
        | Some d -> Filename.concat d f
      in
      let external_obj_dir =
        Obj_dir.convert_to_external obj_dir ~dir:(Path.build dir)
      in
      let cm_dir m cm_kind =
        let visibility = Module.visibility m in
        let dir' = Obj_dir.cm_dir external_obj_dir cm_kind visibility in
        if Path.equal (Path.build dir) dir' then
          None
        else
          Path.basename dir' |> inside_subdir |> Option.some
      in
      let virtual_library = Library.is_virtual lib in
      List.concat_map installable_modules ~f:(fun m ->
          let cm_file kind = Obj_dir.Module.cm_file obj_dir m ~kind in
          let if_ b (cm_kind, f) =
            if b then
              match f with
              | None -> []
              | Some f -> [ (cm_kind, f) ]
            else
              []
          in
          let cm_dir = cm_dir m in
          let other_cm_files =
            let open Cm_kind in
            [ if_ true (Cmi, cm_file Cmi)
            ; if_ native (Cmx, cm_file Cmx)
            ; if_ (byte && virtual_library) (Cmo, cm_file Cmo)
            ; if_
                (native && virtual_library)
                (Cmx, Obj_dir.Module.o_file obj_dir m ~ext_obj)
            ; List.filter_map Ml_kind.all ~f:(fun ml_kind ->
                  let open Option.O in
                  let+ cmt = Obj_dir.Module.cmt_file obj_dir m ~ml_kind in
                  (Cmi, cmt))
            ]
            |> List.concat
            |> List.map ~f:(fun (cm_kind, f) -> (cm_dir cm_kind, f))
          in
          other_cm_files)
    in
    let* lib_files, dll_files =
      let+ lib_files = lib_files ~dir ~dir_contents ~lib_config info in
      let dll_files = dll_files ~modes ~dynlink:lib.dynlink ~ctx info in
      (lib_files, dll_files)
    in
    let+ execs = lib_ppxs sctx ~scope ~lib in
    let install_c_headers =
      List.map
        ~f:(fun base ->
          Path.Build.relative dir (base ^ Foreign_language.header_extension))
        lib.install_c_headers
    in
    List.concat
      [ sources
      ; List.map module_files ~f:(fun (sub_dir, file) ->
            make_entry ?sub_dir Lib file)
      ; List.map lib_files ~f:(fun (section, file) -> make_entry section file)
      ; List.map execs ~f:(make_entry Libexec)
      ; List.map dll_files ~f:(fun a ->
            (Some loc, Install.Entry.make Stublibs a))
      ; List.map ~f:(make_entry Lib) install_c_headers
      ]

  let keep_if expander ~scope stanza =
    let+ keep =
      match (stanza : Stanza.t) with
      | Dune_file.Library lib ->
        if lib.optional then
          Lib.DB.available (Scope.libs scope) (Dune_file.Library.best_name lib)
        else
          Memo.Build.return true
      | Dune_file.Documentation _ -> Memo.Build.return true
      | Dune_file.Install { enabled_if; _ } ->
        Expander.eval_blang expander enabled_if
      | Dune_file.Plugin _ -> Memo.Build.return true
      | Dune_file.Executables ({ install_conf = Some _; _ } as exes) -> (
        Expander.eval_blang expander exes.enabled_if >>= function
        | false -> Memo.Build.return false
        | true ->
          if not exes.optional then
            Memo.Build.return true
          else
            let* compile_info =
              let dune_version =
                Scope.project scope |> Dune_project.dune_version
              in
              let+ pps =
                Resolve.Build.read_memo_build
                  (Preprocess.Per_module.with_instrumentation
                     exes.buildable.preprocess
                     ~instrumentation_backend:
                       (Lib.DB.instrumentation_backend (Scope.libs scope)))
                >>| Preprocess.Per_module.pps
              in
              Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope)
                exes.names exes.buildable.libraries ~pps ~dune_version
                ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
            in
            let+ requires = Lib.Compile.direct_requires compile_info in
            Resolve.is_ok requires)
      | Coq_stanza.Theory.T d -> Memo.Build.return (Option.is_some d.package)
      | _ -> Memo.Build.return false
    in
    Option.some_if keep stanza

  let is_odig_doc_file fn =
    List.exists [ "README"; "LICENSE"; "CHANGE"; "HISTORY" ] ~f:(fun prefix ->
        String.is_prefix fn ~prefix)

  let stanzas_to_entries sctx =
    let ctx = Super_context.context sctx in
    let stanzas = Super_context.stanzas sctx in
    let+ init =
      Package.Name.Map_traversals.parallel_map (Super_context.packages sctx)
        ~f:(fun _name (pkg : Package.t) ->
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
                          (Package.Name.to_string name ^ "/" ^ Findlib.meta_fn)
                    )
                  ; ( None
                    , Install.Entry.make Lib_root dune_package_file
                        ~dst:
                          (Package.Name.to_string name ^ "/" ^ Dune_package.fn)
                    )
                  ])
            in
            let meta_file = Package_paths.meta_file ctx pkg in
            let dune_package_file = Package_paths.dune_package_file ctx pkg in
            (None, Install.Entry.make Lib meta_file ~dst:Findlib.meta_fn)
            ::
            (None, Install.Entry.make Lib dune_package_file ~dst:Dune_package.fn)
            ::
            (if not pkg.has_opam_file then
              deprecated_meta_and_dune_files
            else
              let opam_file = Package_paths.opam_file ctx pkg in
              (None, Install.Entry.make Lib opam_file ~dst:"opam")
              :: deprecated_meta_and_dune_files)
          in
          let pkg_dir = Package.dir pkg in
          Source_tree.find_dir pkg_dir >>| function
          | None -> init
          | Some dir ->
            let pkg_dir = Path.Build.append_source ctx.build_dir pkg_dir in
            Source_tree.Dir.files dir
            |> String.Set.fold ~init ~f:(fun fn acc ->
                   if is_odig_doc_file fn then
                     let odig_file = Path.Build.relative pkg_dir fn in
                     let entry = (None, Install.Entry.make Doc odig_file) in
                     entry :: acc
                   else
                     acc))
    and+ l =
      Dir_with_dune.deep_fold stanzas ~init:[] ~f:(fun d stanza acc ->
          let named_entries =
            let { Dir_with_dune.ctx_dir = dir; scope; _ } = d in
            let* expander = Super_context.expander sctx ~dir in
            let* stanza_and_package =
              let+ stanza = keep_if expander stanza ~scope in
              let open Option.O in
              let* stanza = stanza in
              let+ package = Dune_file.stanza_package stanza in
              (stanza, package)
            in
            match stanza_and_package with
            | None -> Memo.Build.return None
            | Some (stanza, package) ->
              let new_entries =
                match (stanza : Stanza.t) with
                | Dune_file.Install i
                | Dune_file.Executables { install_conf = Some i; _ } ->
                  let path_expander =
                    File_binding.Unexpanded.expand ~dir
                      ~f:(Expander.No_deps.expand_str expander)
                  in
                  let section = i.section in
                  Memo.Build.List.map i.files ~f:(fun unexpanded ->
                      let+ fb = path_expander unexpanded in
                      let loc = File_binding.Expanded.src_loc fb in
                      let src = File_binding.Expanded.src fb in
                      let dst = File_binding.Expanded.dst fb in
                      ( Some loc
                      , Install.Entry.make_with_site section
                          (Super_context.get_site_of_packages sctx)
                          src ?dst ))
                | Dune_file.Library lib ->
                  let sub_dir = Dune_file.Library.sub_dir lib in
                  let* dir_contents = Dir_contents.get sctx ~dir in
                  lib_install_files sctx ~scope ~dir ~sub_dir lib ~dir_contents
                | Coq_stanza.Theory.T coqlib ->
                  Coq_rules.install_rules ~sctx ~dir coqlib
                | Dune_file.Documentation d ->
                  let* dc = Dir_contents.get sctx ~dir in
                  let+ mlds = Dir_contents.mlds dc d in
                  List.map mlds ~f:(fun mld ->
                      ( None
                      , Install.Entry.make
                          ~dst:
                            (sprintf "odoc-pages/%s" (Path.Build.basename mld))
                          Section.Doc mld ))
                | Dune_file.Plugin t -> Plugin_rules.install_rules ~sctx ~dir t
                | _ -> Memo.Build.return []
              in
              let name = Package.name package in
              let+ entries = new_entries in
              Some (name, entries)
          in
          named_entries :: acc)
      |> Memo.Build.parallel_map ~f:Fun.id
    in
    List.fold_left l ~init ~f:(fun acc named_entries ->
        match named_entries with
        | None -> acc
        | Some (name, entries) ->
          Package.Name.Map.Multi.add_all acc name entries)
    |> Package.Name.Map.map ~f:(fun entries ->
           (* Sort entries so that the ordering in [dune-package] is independent
              of Dune's current implementation. *)
           (* jeremiedimino: later on, we group this list by section and sort
              each section. It feels like we should just do this here once and
              for all. *)
           List.sort entries ~compare:(fun (_, a) (_, b) ->
               Install.Entry.compare Path.Build.compare a b))

  let stanzas_to_entries =
    let memo =
      Memo.create
        ~input:(module Super_context.As_memo_key)
        "stanzas-to-entries" stanzas_to_entries
    in
    Memo.exec memo
end

module Meta_and_dune_package : sig
  val meta_and_dune_package_rules :
    Super_context.t -> dir:Path.Build.t -> unit Memo.Build.t
end = struct
  let sections ctx_name files pkg =
    let pkg_name = Package.name pkg in
    let sections =
      (* the one from sites *)
      Section.Site.Map.values pkg.sites |> Section.Set.of_list
    in
    let sections =
      (* the one from install stanza *)
      List.fold_left ~init:sections files ~f:(fun acc (s, _) ->
          Section.Set.add acc s)
    in
    Section.Set.to_map sections ~f:(fun section ->
        Install.Section.Paths.get_local_location ctx_name section pkg_name)

  let make_dune_package sctx lib_entries (pkg : Package.t) =
    let pkg_name = Package.name pkg in
    let ctx = Super_context.context sctx in
    let pkg_root =
      Local_install_path.lib_dir ~context:ctx.name ~package:pkg_name
    in
    let lib_root lib =
      let subdir =
        let name = Lib.name lib in
        let _, subdir = Lib_name.split name in
        match
          let info = Lib.info lib in
          Lib_info.status info
        with
        | Private (_, Some _) ->
          Lib_name.Local.mangled_path_under_package (Lib_name.to_local_exn name)
          @ subdir
        | _ -> subdir
      in
      Path.Build.L.relative pkg_root subdir
    in
    let* entries =
      Memo.Build.parallel_map lib_entries ~f:(fun stanza ->
          match stanza with
          | Super_context.Lib_entry.Deprecated_library_name
              { old_name = _, Deprecated _; _ } ->
            Memo.Build.return None
          | Super_context.Lib_entry.Deprecated_library_name
              { old_name = old_public_name, Not_deprecated
              ; new_public_name = _, new_public_name
              ; loc
              ; project = _
              } ->
            let old_public_name = Dune_file.Public_lib.name old_public_name in
            Memo.Build.return
              (Some
                 ( old_public_name
                 , Dune_package.Entry.Deprecated_library_name
                     { loc; old_public_name; new_public_name } ))
          | Library lib ->
            let* dir_contents =
              let info = Lib.Local.info lib in
              let dir = Lib_info.src_dir info in
              Dir_contents.get sctx ~dir
            in
            let obj_dir = Lib.Local.obj_dir lib in
            let lib = Lib.Local.to_lib lib in
            let name = Lib.name lib in
            let* foreign_objects =
              (* We are writing the list of .o files to dune-package, but we
                 actually only install them for virtual libraries. See
                 [Lib_archives.make] *)
              let dir = Obj_dir.obj_dir obj_dir in
              let+ foreign_sources =
                Dir_contents.foreign_sources dir_contents
              in
              foreign_sources
              |> Foreign_sources.for_lib ~name
              |> Foreign.Sources.object_files ~dir
                   ~ext_obj:ctx.lib_config.ext_obj
              |> List.map ~f:Path.build
            and* modules =
              Dir_contents.ocaml dir_contents
              >>| Ml_sources.modules ~for_:(Library name)
            in
            let+ sub_systems =
              Lib.to_dune_lib lib
                ~dir:(Path.build (lib_root lib))
                ~modules ~foreign_objects
              >>= Resolve.read_memo_build
            in
            Some (name, Dune_package.Entry.Library sub_systems))
    in
    let entries =
      List.fold_left entries ~init:Lib_name.Map.empty ~f:(fun acc x ->
          match x with
          | None -> acc
          | Some (name, x) -> Lib_name.Map.add_exn acc name x)
    in
    let+ files =
      let+ map = Stanzas_to_entries.stanzas_to_entries sctx in
      Package.Name.Map.Multi.find map pkg_name
      |> List.map ~f:(fun (_, entry) ->
             (entry.Install.Entry.section, entry.dst))
      |> Section.Map.of_list_multi |> Section.Map.to_list
    in
    let sections = sections ctx.name files pkg in
    Dune_package.Or_meta.Dune_package
      { Dune_package.version = pkg.version
      ; name = pkg_name
      ; entries
      ; dir = Path.build pkg_root
      ; sections
      ; sites = pkg.sites
      ; files
      }

  let gen_dune_package sctx (pkg : Package.t) =
    let ctx = Super_context.context sctx in
    let dune_version =
      Dune_lang.Syntax.greatest_supported_version Stanza.syntax
    in
    let lib_entries =
      Super_context.lib_entries_of_package sctx (Package.name pkg)
    in
    let action =
      let dune_package_file = Package_paths.dune_package_file ctx pkg in
      let meta_template = Package_paths.meta_template ctx pkg in
      Action_builder.write_file_dyn dune_package_file
        (let open Action_builder.O in
        let+ pkg =
          Action_builder.if_file_exists (Path.build meta_template)
            ~then_:(Action_builder.return Dune_package.Or_meta.Use_meta)
            ~else_:
              (Action_builder.memo_build
                 (Memo.Build.bind (Memo.Build.return ()) ~f:(fun () ->
                      make_dune_package sctx lib_entries pkg)))
        in
        Format.asprintf "%a" (Dune_package.Or_meta.pp ~dune_version) pkg)
    in
    let deprecated_dune_packages =
      List.filter_map lib_entries ~f:(function
        | Super_context.Lib_entry.Deprecated_library_name
            ({ old_name = old_public_name, Deprecated _; _ } as t) ->
          Some
            ( Lib_name.package_name (Dune_file.Public_lib.name old_public_name)
            , t )
        | _ -> None)
      |> Package.Name.Map.of_list_multi
    in
    let* () =
      Package.Name.Map.foldi pkg.deprecated_package_names
        ~init:(Memo.Build.return ()) ~f:(fun name _ acc ->
          acc
          >>>
          let dune_pkg =
            let entries =
              match Package.Name.Map.find deprecated_dune_packages name with
              | None -> Lib_name.Map.empty
              | Some entries ->
                List.fold_left entries ~init:Lib_name.Map.empty
                  ~f:(fun
                       acc
                       { Dune_file.Library_redirect.old_name =
                           old_public_name, _
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
            let sections = sections ctx.name [] pkg in
            { Dune_package.version = pkg.version
            ; name
            ; entries
            ; dir =
                Path.build
                  (Local_install_path.lib_dir ~context:ctx.name ~package:name)
            ; sections
            ; sites = pkg.sites
            ; files = []
            }
          in
          let action_with_targets =
            Action_builder.write_file
              (Package_paths.deprecated_dune_package_file ctx pkg
                 dune_pkg.Dune_package.name)
              (Format.asprintf "%a"
                 (Dune_package.Or_meta.pp ~dune_version)
                 (Dune_package.Or_meta.Dune_package dune_pkg))
          in
          Super_context.add_rule sctx ~dir:ctx.build_dir action_with_targets)
    in
    Super_context.add_rule sctx ~dir:ctx.build_dir action

  let gen_meta_file sctx (pkg : Package.t) =
    let ctx = Super_context.context sctx in
    let pkg_name = Package.name pkg in
    let deprecated_packages, entries =
      let entries = Super_context.lib_entries_of_package sctx pkg_name in
      List.partition_map entries ~f:(function
        | Super_context.Lib_entry.Deprecated_library_name
            { old_name = public, Deprecated { deprecated_package }; _ } as entry
          -> (
          match Dune_file.Public_lib.sub_dir public with
          | None -> Left (deprecated_package, entry)
          | Some _ -> Right entry)
        | entry -> Right entry)
    in
    let template =
      let meta_template = Path.build (Package_paths.meta_template ctx pkg) in
      let meta_template_lines_or_fail =
        (* XXX this should really be lazy as it's only necessary for the then
           clause. There's no way to express this in the action builder however. *)
        let vlib =
          List.find_map entries ~f:(function
            | Super_context.Lib_entry.Library lib ->
              let info = Lib.Local.info lib in
              Option.some_if (Option.is_some (Lib_info.virtual_ info)) lib
            | Deprecated_library_name _ -> None)
        in
        match vlib with
        | None -> Action_builder.lines_of meta_template
        | Some vlib ->
          Action_builder.fail
            { fail =
                (fun () ->
                  let name = Lib.name (Lib.Local.to_lib vlib) in
                  User_error.raise
                    ~loc:(Loc.in_file meta_template)
                    [ Pp.textf
                        "Package %s defines virtual library %s and has a META \
                         template. This is not allowed."
                        (Package.Name.to_string pkg_name)
                        (Lib_name.to_string name)
                    ])
            }
      in
      Action_builder.if_file_exists meta_template
        ~then_:meta_template_lines_or_fail
        ~else_:(Action_builder.return [ "# DUNE_GEN" ])
    in
    let ctx = Super_context.context sctx in
    let meta = Package_paths.meta_file ctx pkg in
    let* () =
      Super_context.add_rule sctx ~dir:ctx.build_dir
        (let open Action_builder.O in
        (let* template = template in
         let+ meta =
           Action_builder.memo_build
             (Gen_meta.gen ~package:pkg ~add_directory_entry:true entries)
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
         Format.asprintf "%a" Pp.to_fmt pp)
        |> Action_builder.write_file_dyn meta)
    in
    let deprecated_packages =
      Package.Name.Map.of_list_multi deprecated_packages
    in
    Package.Name.Map_traversals.parallel_iter pkg.deprecated_package_names
      ~f:(fun name _loc ->
        let meta = Package_paths.deprecated_meta_file ctx pkg name in
        Super_context.add_rule sctx ~dir:ctx.build_dir
          (Action_builder.write_file_dyn meta
             (let open Action_builder.O in
             let+ meta =
               let entries =
                 match Package.Name.Map.find deprecated_packages name with
                 | None -> []
                 | Some entries -> entries
               in
               Action_builder.memo_build
                 (Gen_meta.gen ~package:pkg entries ~add_directory_entry:false)
             in
             let pp =
               let open Pp.O in
               Pp.vbox (Meta.pp meta.entries ++ Pp.cut)
             in
             Format.asprintf "%a" Pp.to_fmt pp)))

  let meta_and_dune_package_rules_impl (project, sctx) =
    Dune_project.packages project
    |> Package.Name.Map_traversals.parallel_iter
         ~f:(fun _name (pkg : Package.t) ->
           gen_dune_package sctx pkg >>> gen_meta_file sctx pkg)

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
      ~implicit_output:Rules.implicit_output meta_and_dune_package_rules_impl

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
  let install_dir = Local_install_path.dir ~context:ctx.name in
  Memo.Build.parallel_map entries ~f:(fun (loc, entry) ->
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
      let+ () =
        Super_context.add_rule sctx ~loc ~dir:ctx.build_dir
          (Action_builder.symlink ~src:(Path.build entry.src) ~dst)
      in
      Install.Entry.set_src entry (Path.build dst))

let promote_install_file (ctx : Context.t) =
  !Clflags.promote_install_files
  && (not ctx.implicit)
  &&
  match ctx.kind with
  | Default -> true
  | Opam _ -> false

let install_entries sctx (package : Package.t) =
  let+ packages = Stanzas_to_entries.stanzas_to_entries sctx in
  Package.Name.Map.Multi.find packages (Package.name package)

let packages =
  let f sctx =
    let packages = Package.Name.Map.values (Super_context.packages sctx) in
    let+ l =
      Memo.Build.parallel_map packages ~f:(fun (pkg : Package.t) ->
          install_entries sctx pkg
          >>| List.map ~f:(fun (_loc, (entry : _ Install.Entry.t)) ->
                  (entry.src, pkg.id)))
    in
    Path.Build.Map.of_list_fold (List.concat l) ~init:Package.Id.Set.empty
      ~f:Package.Id.Set.add
  in
  let memo =
    Memo.create "package-map"
      ~input:(module Super_context.As_memo_key)
      ~cutoff:(Path.Build.Map.equal ~equal:Package.Id.Set.equal)
      f
  in
  fun sctx -> Memo.exec memo sctx

let packages_file_is_part_of path =
  Memo.Build.Option.bind
    (let open Option.O in
    let* ctx_name, _ = Path.Build.extract_build_context path in
    Context_name.of_string_opt ctx_name)
    ~f:Super_context.find
  >>= function
  | None -> Memo.Build.return Package.Id.Set.empty
  | Some sctx ->
    let open Memo.Build.O in
    let+ map = packages sctx in
    Option.value (Path.Build.Map.find map path) ~default:Package.Id.Set.empty

let install_rules sctx (package : Package.t) =
  let package_name = Package.name package in
  let install_paths =
    Install.Section.Paths.make ~package:package_name ~destdir:Path.root ()
  in
  let* entries =
    install_entries sctx package
    >>= symlink_installed_artifacts_to_build_install sctx ~install_paths
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
    let open Action_builder.O in
    let+ packages =
      Action_builder.memo_build
        (Build_system.package_deps package files
           ~packages_of:packages_file_is_part_of)
    in
    match strict_package_deps with
    | false -> packages
    | true ->
      let missing_deps =
        let effective_deps =
          Package.Id.Set.to_list packages
          |> Package.Name.Set.of_list_map ~f:Package.Id.name
        in
        Package.missing_deps package ~effective_deps
      in
      if Package.Name.Set.is_empty missing_deps then
        packages
      else
        let name = Package.name package in
        User_error.raise
          [ Pp.textf "Package %s is missing the following package dependencies"
              (Package.Name.to_string name)
          ; Package.Name.Set.to_list missing_deps
            |> Pp.enumerate ~f:(fun name ->
                   Pp.text (Package.Name.to_string name))
          ]
  in
  let* () =
    let context = Context.build_context ctx in
    let target_alias = Alias.package_install ~context ~pkg:package in
    let open Action_builder.O in
    Rules.Produce.Alias.add_deps target_alias
      (Action_builder.dyn_deps
         (let+ packages = packages
          and+ () = Action_builder.deps (Dep.Set.of_files_set files) in
          ( ()
          , Package.Id.Set.to_list packages
            |> Dep.Set.of_list_map ~f:(fun (pkg : Package.Id.t) ->
                   let pkg =
                     let name = Package.Id.name pkg in
                     Package.Name.Map.find_exn
                       (Super_context.packages sctx)
                       name
                   in
                   Alias.package_install ~context ~pkg |> Dep.alias) )))
  in
  let action =
    let install_file =
      Path.Build.relative pkg_build_dir
        (Utils.install_file ~package:package_name
           ~findlib_toolchain:ctx.findlib_toolchain)
    in
    let open Action_builder.O in
    Action_builder.write_file_dyn install_file
      (let+ () = Action_builder.path_set files
       and+ () =
         if strict_package_deps then
           Action_builder.map packages ~f:(fun (_ : Package.Id.Set.t) -> ())
         else
           Action_builder.return ()
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
      (if promote_install_file ctx then
        Promote { lifetime = Until_clean; into = None; only = None }
      else
        (* We must ignore the source file since it might be copied to the source
           tree by another context. *)
        Ignore_source_files)
    action

let memo =
  let module Sctx_and_package = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Package.t

    let hash (x, y) = Hashtbl.hash (Super_context.hash x, Package.hash y)

    let equal (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

    let to_dyn _ = Dyn.Opaque
  end in
  let install_alias (ctx : Context.t) (package : Package.t) =
    let name = Package.name package in
    if not ctx.implicit then
      let install_fn =
        Utils.install_file ~package:name
          ~findlib_toolchain:ctx.findlib_toolchain
      in
      let path = Package_paths.build_dir ctx package in
      let install_alias = Alias.install ~dir:path in
      let install_file = Path.relative (Path.build path) install_fn in
      Rules.Produce.Alias.add_deps install_alias
        (Action_builder.path install_file)
    else
      Memo.Build.return ()
  in
  Memo.create
    ~input:(module Sctx_and_package)
    "install-rules-and-pkg-entries"
    (fun (sctx, pkg) ->
      Memo.Build.return
        (let ctx = Super_context.context sctx in
         let context_name = ctx.name in
         let rules =
           Memo.lazy_ (fun () ->
               Rules.collect_unit (fun () ->
                   let* () = install_rules sctx pkg in
                   install_alias ctx pkg))
         in
         Scheme.Approximation
           ( Dir_set.union_all
               [ Dir_set.subtree (Local_install_path.dir ~context:context_name)
               ; Dir_set.singleton (Package_paths.build_dir ctx pkg)
               ]
           , Thunk
               (fun () ->
                 let+ rules = Memo.Lazy.force rules in
                 Scheme.Finite (Rules.to_map rules)) )))

let scheme sctx pkg = Memo.exec memo (sctx, pkg)

let scheme_per_ctx_memo =
  Memo.create
    ~input:(module Super_context.As_memo_key)
    "install-rule-scheme"
    (fun sctx ->
      let packages = Package.Name.Map.values (Super_context.packages sctx) in
      let* schemes = Memo.Build.sequential_map packages ~f:(scheme sctx) in
      Scheme.evaluate ~union:Rules.Dir_rules.union (Scheme.all schemes))

let gen_rules sctx ~dir =
  let* rules, subdirs =
    let* scheme = Memo.exec scheme_per_ctx_memo sctx in
    Scheme.Evaluated.get_rules scheme ~dir
  in
  let+ () =
    Rules.produce_dir ~dir (Option.value ~default:Rules.Dir_rules.empty rules)
  in
  Build_system.Subdir_set.These subdirs
