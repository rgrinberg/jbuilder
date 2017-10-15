open Import
open Jbuild
open Build.O
open! No_io

module type Params = sig
  val sctx : Super_context.t
  val modules_by_lib : (string, Module.t list) Hashtbl.t
end

module Gen(P : Params) = struct
  module SC = Super_context
  open P

  let ctx = SC.context sctx

  (* this is copy pasta *)
  let lib_archive (lib : Library.t) ~dir ~ext = Path.relative dir (lib.name ^ ext)

  let stubs_archive lib ~dir =
    Library.stubs_archive lib ~dir ~ext_lib:ctx.ext_lib

  let dll (lib : Library.t) ~dir =
    Path.relative dir (sprintf "dll%s_stubs%s" lib.name ctx.ext_dll)

  (* +-----------------------------------------------------------------+
     | META                                                            |
     +-----------------------------------------------------------------+ *)

  (* The rules for META files must come after the interpretation of the jbuild stanzas
     since a user rule might generate a META.<package> file *)

  (* META files that must be installed. Either because there is an explicit or user
     generated one, or because *)
  let packages_with_explicit_or_user_generated_meta =
    String_map.values (SC.packages sctx)
    |> List.filter_map ~f:(fun (pkg : Package.t) ->
      let path = Path.append ctx.build_dir pkg.path in
      let meta_fn = "META." ^ pkg.name in
      let meta_templ_fn = meta_fn ^ ".template" in

      let files =
        SC.sources_and_targets_known_so_far sctx ~src_path:pkg.path
      in
      let has_meta, has_meta_tmpl =
        (String_set.mem meta_fn files,
         String_set.mem meta_templ_fn files)
      in

      let meta_fn =
        if has_meta then
          meta_fn ^ ".from-jbuilder"
        else
          meta_fn
      in
      let meta_path = Path.relative path meta_fn in

      let version =
        let get =
          match pkg.version_from_opam_file with
          | Some s -> Build.return (Some s)
          | None ->
            let rec loop = function
              | [] -> Build.return None
              | candidate :: rest ->
                let p = Path.relative path candidate in
                Build.if_file_exists p
                  ~then_:(Build.lines_of p
                          >>^ function
                          | ver :: _ -> Some ver
                          | _ -> Some "")
                  ~else_:(loop rest)
            in
            loop
              [ pkg.name ^ ".version"
              ; "version"
              ; "VERSION"
              ]
        in
        Super_context.Pkg_version.set sctx pkg get
      in

      let template =
        if has_meta_tmpl then
          let meta_templ_path = Path.relative path meta_templ_fn in
          Build.lines_of meta_templ_path
        else
          Build.return ["# JBUILDER_GEN"]
      in
      let meta =
        Gen_meta.gen ~package:pkg.name
          ~version
          ~stanzas:(SC.stanzas_to_consider_for_install sctx)
          ~lib_deps:(fun ~dir jbuild ->
            match jbuild with
            | Library lib ->
              Build.arr ignore
              >>>
              SC.Libs.load_requires sctx ~dir ~item:lib.name
              >>^ List.map ~f:Lib.best_name
            | Executables exes ->
              let item = List.hd exes.names in
              Build.arr ignore
              >>>
              SC.Libs.load_requires sctx ~dir ~item
              >>^ List.map ~f:Lib.best_name
            | _ -> Build.arr (fun _ -> []))
          ~ppx_runtime_deps:(fun ~dir jbuild ->
            match jbuild with
            | Library lib ->
              Build.arr ignore
              >>>
              SC.Libs.load_runtime_deps sctx ~dir ~item:lib.name
              >>^ List.map ~f:Lib.best_name
            | _ -> Build.arr (fun _ -> []))
      in
      SC.add_rule sctx
        (Build.fanout meta template
         >>^ (fun ((meta : Meta.t), template) ->
           let buf = Buffer.create 1024 in
           let ppf = Format.formatter_of_buffer buf in
           Format.pp_open_vbox ppf 0;
           List.iter template ~f:(fun s ->
             if String.is_prefix s ~prefix:"#" then
               match
                 String.extract_blank_separated_words
                   (String.sub s ~pos:1 ~len:(String.length s - 1))
               with
               | ["JBUILDER_GEN"] -> Format.fprintf ppf "%a@," Meta.pp meta.entries
               | _ -> Format.fprintf ppf "%s@," s
             else
               Format.fprintf ppf "%s@," s);
           Format.pp_close_box ppf ();
           Format.pp_print_flush ppf ();
           Buffer.contents buf)
         >>>
         Build.write_file_dyn meta_path);

      if has_meta || has_meta_tmpl then
        Some pkg.name
      else
        None)
    |> String_set.of_list

  (* +-----------------------------------------------------------------+
     | Installation                                                    |
     +-----------------------------------------------------------------+ *)

  let lib_install_files ~dir ~sub_dir (lib : Library.t) =
    let make_entry section fn =
      Install.Entry.make section fn
        ?dst:(Option.map sub_dir ~f:(fun d -> sprintf "%s/%s" d (Path.basename fn)))
    in
    let { Mode.Dict. byte; native } = lib.modes in
    let if_ cond l = if cond then l else [] in
    let files =
      let modules =
        Hashtbl.find_exn modules_by_lib lib.name
          ~string_of_key:(sprintf "%S")
          ~table_desc:(fun _ ->
            sprintf "<module table for context %s>"
              (Path.to_string ctx.build_dir))
      in
      List.concat
        [ List.concat_map modules ~f:(fun m ->
            List.concat
              [ [ Module.cm_file m ~dir Cmi ]
              ; if_ native [ Module.cm_file m ~dir Cmx ]
              ; List.filter_map Ml_kind.all ~f:(Module.cmt_file m ~dir)
              ; [ match Module.file m ~dir Intf with
                  | Some fn -> fn
                  | None    -> Path.relative dir m.impl.name ]
              ])
        ; if_ byte [ lib_archive ~dir lib ~ext:".cma" ]
        ; if_ (Library.has_stubs lib) [ stubs_archive ~dir lib ]
        ; if_ native
            (match ctx.ocamlopt with
             | None -> []
             | Some _ ->
               let files =
                 [ lib_archive ~dir lib ~ext:".cmxa"
                 ; lib_archive ~dir lib ~ext:ctx.ext_lib
                 ]
               in
               if ctx.natdynlink_supported && lib.dynlink then
                 files @ [ lib_archive ~dir lib ~ext:".cmxs" ]
               else
                 files
            )
        ; List.map lib.buildable.js_of_ocaml.javascript_files ~f:(Path.relative dir)
        ; List.map lib.install_c_headers ~f:(fun fn ->
            Path.relative dir (fn ^ ".h"))
        ]
    in
    let dlls  = if_ (byte && Library.has_stubs lib && lib.dynlink) [dll ~dir lib] in
    let execs =
      match lib.kind with
      | Normal | Ppx_deriver -> []
      | Ppx_rewriter ->
        let pps = [Pp.of_string lib.name] in
        let pps =
          (* This is a temporary hack until we get a standard driver *)
          let deps = List.concat_map lib.buildable.libraries ~f:Lib_dep.to_lib_names in
          if List.exists deps ~f:(function
            | "ppx_driver" | "ppx_type_conv" -> true
            | _ -> false) then
            pps @ [Pp.of_string "ppx_driver.runner"]
          else
            pps
        in
        let ppx_exe =
          SC.PP.get_ppx_driver sctx pps
            ~dir ~dep_kind:(if lib.optional then Build.Optional else Required)
        in
        [ppx_exe]
    in
    List.concat
      [ List.map files ~f:(make_entry Lib    )
      ; List.map execs ~f:(make_entry Libexec)
      ; List.map dlls  ~f:(Install.Entry.make Stublibs)
      ]

  let is_odig_doc_file fn =
    List.exists [ "README"; "LICENSE"; "CHANGE"; "HISTORY"]
      ~f:(fun prefix -> String.is_prefix fn ~prefix)

  let local_install_rules (entries : Install.Entry.t list) ~package =
    let install_dir = Config.local_install_dir ~context:ctx.name in
    List.map entries ~f:(fun entry ->
      let dst =
        Path.append install_dir (Install.Entry.relative_installed_path entry ~package)
      in
      SC.add_rule sctx (Build.symlink ~src:entry.src ~dst);
      Install.Entry.set_src entry dst)

  let install_file package_path package entries =
    let entries =
      let files = SC.sources_and_targets_known_so_far sctx ~src_path:Path.root in
      String_set.fold files ~init:entries ~f:(fun fn acc ->
        if is_odig_doc_file fn then
          Install.Entry.make Doc (Path.relative ctx.build_dir fn) :: acc
        else
          acc)
    in
    let entries =
      let opam = Path.relative package_path (package ^ ".opam") in
      Install.Entry.make Lib opam ~dst:"opam" :: entries
    in
    let entries =
      (* Install a META file if the user wrote one or setup a rule to generate one, or if
         we have at least another file to install in the lib/ directory *)
      let meta_fn = "META." ^ package in
      if String_set.mem package packages_with_explicit_or_user_generated_meta ||
         List.exists entries ~f:(fun (e : Install.Entry.t) -> e.section = Lib) then
        let meta = Path.append ctx.build_dir (Path.relative package_path meta_fn) in
        Install.Entry.make Lib meta ~dst:"META" :: entries
      else
        entries
    in
    let fn =
      Path.relative (Path.append ctx.build_dir package_path) (package ^ ".install")
    in
    let entries = local_install_rules entries ~package in
    SC.add_rule sctx
      (Build.path_set (Install.files entries)
       >>^ (fun () ->
         Install.gen_install_file entries)
       >>>
       Build.write_file_dyn fn)

  let () =
    let entries_per_package =
      List.concat_map (SC.stanzas_to_consider_for_install sctx)
        ~f:(fun (dir, stanza) ->
          match stanza with
          | Library ({ public = Some { package; sub_dir; _ }; _ } as lib) ->
            List.map (lib_install_files ~dir ~sub_dir lib) ~f:(fun x ->
              package.name, x)
          | Install { section; files; package}->
            List.map files ~f:(fun { Install_conf. src; dst } ->
              (package.name, Install.Entry.make section (Path.relative dir src) ?dst))
          | _ -> [])
      |> String_map.of_alist_multi
    in
    String_map.iter (SC.packages sctx) ~f:(fun ~key:_ ~data:(pkg : Package.t) ->
      let stanzas = String_map.find_default pkg.name entries_per_package ~default:[] in
      install_file pkg.path pkg.name stanzas)

  let () =
    let is_default = Path.basename ctx.build_dir = "default" in
    String_map.iter (SC.packages sctx)
      ~f:(fun ~key:pkg ~data:{ Package.path = src_path; _ } ->
        let install_fn = pkg ^ ".install" in

        let ctx_path = Path.append ctx.build_dir src_path in
        let ctx_install_alias = Alias.install ~dir:ctx_path in
        let ctx_install_file = Path.relative ctx_path install_fn in
        Alias.add_deps (SC.aliases sctx) ctx_install_alias [ctx_install_file];

        if is_default then begin
          let src_install_alias = Alias.install ~dir:src_path in
          let src_install_file = Path.relative src_path install_fn in
          SC.add_rule sctx (Build.copy ~src:ctx_install_file ~dst:src_install_file);
          Alias.add_deps (SC.aliases sctx) src_install_alias [src_install_file]
        end)

end

let setup ~modules_by_lib sctx =
  let module M =
    Gen(struct
      let sctx = sctx
      let modules_by_lib = modules_by_lib
    end) in
  ()
