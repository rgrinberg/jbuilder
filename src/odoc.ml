open Import
open Jbuild
open Build.O

module SC = Super_context

let (++) = Path.relative

let lib_unique_name lib =
  let name = Lib.name lib in
  match Lib.status lib with
  | Installed -> assert false
  | Public    -> name
  | Private scope_name ->
    sprintf "%s@%s" name (Scope_info.Name.to_string scope_name)

let pkg_or_lnu lib =
  match Lib.pkg lib with
  | Some p -> Package.Name.to_string p
  | None -> lib_unique_name lib

type target =
  | Lib of Lib.t
  | Pkg of Package.Name.t

module Paths = struct
  let root sctx = (SC.context sctx).Context.build_dir ++ "_doc"

  let odocs sctx m =
    root sctx ++ (
      match m with
      | Lib lib -> sprintf "_odoc/lib/%s" (lib_unique_name lib)
      | Pkg pkg -> sprintf "_odoc/pkg/%s" (Package.Name.to_string pkg)
    )

  let html_root sctx = root sctx ++ "_html"

  let html sctx m =
    html_root sctx ++ (
      match m with
      | Pkg pkg -> Package.Name.to_string pkg
      | Lib lib -> pkg_or_lnu lib
    )

  let gen_mld_dir sctx (pkg : Package.t) =
    root sctx ++ "_mlds" ++ (Package.Name.to_string pkg.name)
end

module Dep = struct
  let html_alias sctx m =
    Build_system.Alias.doc ~dir:(Paths.html sctx m)

  let alias = Build_system.Alias.make ".odoc-all"

  let deps t =
    Build.dyn_paths (Build.arr (
      List.fold_left ~init:[] ~f:(fun acc (lib : Lib.t) ->
        if Lib.is_local lib then (
          let dir = Paths.odocs t (Lib lib) in
          Build_system.Alias.stamp_file (alias ~dir) :: acc
        ) else (
          acc
        )
      )))

  let alias t m = alias ~dir:(Paths.odocs t m)

  (* let static_deps t lib = Build_system.Alias.dep (alias t lib) *)

  let setup_deps t m files = SC.add_alias_deps t (alias t m) files
end

let get_odoc sctx = SC.resolve_program sctx "odoc" ~hint:"opam install odoc"
let odoc_ext = ".odoc"

module Mld : sig
  type t

  val create : Path.t -> t

  val odoc_file : doc_dir:Path.t -> t -> Path.t
  val odoc_input : t -> Path.t

end = struct
  type t = Path.t

  let create p = p

  let odoc_file ~doc_dir t =
    let t = Filename.chop_extension (Path.basename t) in
    Path.relative doc_dir (sprintf "page-%s%s" t odoc_ext)

  let odoc_input t = t
end

let module_deps (m : Module.t) ~doc_dir ~(dep_graphs:Ocamldep.Dep_graphs.t) =
  Build.dyn_paths
    ((match m.intf with
       | Some _ ->
         Ocamldep.Dep_graph.deps_of dep_graphs.intf m
       | None ->
         (* When a module has no .mli, use the dependencies for the .ml *)
         Ocamldep.Dep_graph.deps_of dep_graphs.impl m)
     >>^ List.map ~f:(Module.odoc_file ~doc_dir))

let compile_module sctx (m : Module.t) ~odoc ~dir ~obj_dir ~includes ~dep_graphs
      ~doc_dir ~pkg_or_lnu =
  let context = SC.context sctx in
  let odoc_file = Module.odoc_file m ~doc_dir in
  SC.add_rule sctx
    (module_deps m ~doc_dir ~dep_graphs
     >>>
     includes
     >>>
     Build.run ~context ~dir:doc_dir odoc
       [ A "compile"
       ; A "-I"; Path dir
       ; Dyn (fun x -> x)
       ; As ["--pkg"; pkg_or_lnu]
       ; A "-o"; Target odoc_file
       ; Dep (Module.cmti_file m ~obj_dir)
       ]);
  (m, odoc_file)

let compile_mld sctx (m : Mld.t) ~odoc ~includes ~doc_dir ~pkg =
  let context = SC.context sctx in
  let odoc_file = Mld.odoc_file m ~doc_dir in
  SC.add_rule sctx
    (includes
     >>>
     Build.run ~context ~dir:doc_dir odoc
       [ A "compile"
       ; Dyn (fun x -> x)
       ; As ["--pkg"; Package.Name.to_string pkg]
       ; A "-o"; Target odoc_file
       ; Dep (Mld.odoc_input m)
       ]);
  odoc_file

type odoc =
  { odoc_input: Path.t
  ; html_dir: Path.t
  ; html_file: Path.t
  ; html_alias: Build_system.Alias.t
  ; typ: [`Module | `Mld]
  }

let odoc_include_flags sctx libs =
  let paths =
    libs |> List.fold_left ~f:(fun paths lib ->
      if Lib.is_local lib then (
        Path.Set.add paths (Paths.odocs sctx (Lib lib))
      ) else (
        paths
      )
    ) ~init:Path.Set.empty in
  Arg_spec.S (List.concat_map (Path.Set.to_list paths)
                ~f:(fun dir -> [Arg_spec.A "-I"; Path dir]))

let to_html sctx (odoc_file : odoc) ~(odoc : Action.Prog.t) ~deps =
  let context = SC.context sctx in
  let to_remove, jbuilder_keep =
    match odoc_file.typ with
    | `Mld -> odoc_file.html_file, []
    | `Module ->
      let jbuilder_keep =
        Build.create_file (odoc_file.html_dir ++ Config.jbuilder_keep_fname) in
      odoc_file.html_dir, [jbuilder_keep]
  in
  SC.add_rule sctx
    (deps
     >>> Build.progn (
       Build.remove_tree to_remove
       :: Build.mkdir odoc_file.html_dir
       :: Build.run ~context ~dir:(Paths.html_root sctx)
            odoc ~extra_targets:[odoc_file.html_file]
            [ A "html"
            ; Dyn (odoc_include_flags sctx)
            ; A "-o"; Path (Paths.html_root sctx)
            ; Dep odoc_file.odoc_input
            ]
       :: jbuilder_keep
     )
    );
  odoc_file.html_file

let css_file ~doc_dir = doc_dir ++ "odoc.css"

let toplevel_index ~doc_dir = doc_dir ++ "index.html"

let setup_library_odoc_rules sctx (library : Library.t) ~dir ~scope ~modules
      ~requires ~(dep_graphs:Ocamldep.Dep_graph.t Ml_kind.Dict.t) =
  let lib =
    Option.value_exn (Lib.DB.find_even_when_hidden (Scope.libs scope)
                        library.name) in
  (* Using the proper package name doesn't actually work since odoc assumes
     that a package contains only 1 library *)
  let pkg_or_lnu = pkg_or_lnu lib in
  let doc_dir = Paths.odocs sctx (Lib lib) in
  let obj_dir = Lib.obj_dir lib in
  let odoc = get_odoc sctx in
  let includes =
    let ctx = SC.context sctx in
    Build.memoize "includes"
      (requires
       >>> Dep.deps sctx
       >>^ Lib.L.include_flags ~stdlib_dir:ctx.stdlib_dir)
  in
  let modules_and_odoc_files =
    List.map (Module.Name.Map.values modules) ~f:(fun m ->
      compile_module sctx ~odoc ~dir ~obj_dir ~includes ~dep_graphs
        ~doc_dir ~pkg_or_lnu m)
  in
  Dep.setup_deps sctx (Lib lib) (List.map modules_and_odoc_files ~f:snd)

let setup_css_rule sctx =
  let context = SC.context sctx in
  let doc_dir = Paths.html_root sctx in
  SC.add_rule sctx
    (Build.run ~context
       ~dir:context.build_dir
       ~extra_targets:[css_file ~doc_dir]
       (get_odoc sctx)
       [ A "css"; A "-o"; Path doc_dir ])

let sp = Printf.sprintf

let setup_toplevel_index_rule sctx =
  let list_items =
    Super_context.packages sctx
    |> Package.Name.Map.to_list
    |> List.filter_map ~f:(fun (name, pkg) ->
      let name = Package.Name.to_string name in
      let link = sp {|<a href="%s/index.html">%s</a>|} name name in
      let version_suffix =
        match pkg.Package.version_from_opam_file with
        | None ->
          ""
        | Some v ->
          sp {| <span class="version">%s</span>|} v
      in
      Some (sp "<li>%s%s</li>" link version_suffix))
  in
  let list_items = String.concat ~sep:"\n    " list_items in
  let html =
    sp {|<!DOCTYPE html>
         <html xmlns="http://www.w3.org/1999/xhtml">
         <head>
         <title>index</title>
         <link rel="stylesheet" href="./odoc.css"/>
         <meta charset="utf-8"/>
         <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
         </head>
         <body>
         <div class="by-name">
         <h2>OCaml package documentation</h2>
         <ol>
         %s
         </ol>
         </body>
         </html>
       |} list_items
  in
  let doc_dir = Paths.html_root sctx in
  SC.add_rule sctx @@ Build.write_file (toplevel_index ~doc_dir) html


let html_alias sctx pkg =
  Build_system.Alias.doc ~dir:(
    let ctx = Super_context.context sctx in
    Path.append ctx.build_dir pkg.Package.path
  )

let read_unique_name sctx lib_unique_name =
  match String.rsplit2 lib_unique_name ~on:'@' with
  | None ->
    (lib_unique_name, SC.public_libs sctx)
  | Some (lib, name) ->
    (lib,
     Scope.libs (SC.find_scope_by_name sctx (Scope_info.Name.of_string name)))

let db_of_pkg sctx ~(pkg : Package.t) =
  Path.append (SC.context sctx).build_dir pkg.path
  |> SC.find_scope_by_dir sctx
  |> Scope.libs

let libs_of_pkg lib_db ~pkg =
  Lib.DB.all lib_db
  |> Lib.Set.filter ~f:(fun lib ->
    Lib.is_local lib && (match Lib.pkg lib with
      | None -> false
      | Some pkg' -> pkg' = pkg))

let load_all_odoc_rules_pkg sctx ~pkg ~lib_db =
  let pkg_libs = libs_of_pkg ~pkg lib_db in
  SC.load_dir sctx ~dir:(Paths.odocs sctx (Pkg pkg));
  Lib.Set.iter pkg_libs ~f:(fun lib ->
    SC.load_dir sctx ~dir:(Paths.odocs sctx (Lib lib)));
  pkg_libs

let create_odoc sctx ~odoc_input m =
  let html_alias = Dep.html_alias sctx m in
  let html_base = Paths.html sctx m in
  match m with
  | Lib _ ->
    let html_dir =
      html_base ++ (
        Path.basename odoc_input
        |> Filename.chop_extension
        |> Stdune.String.capitalize
      ) in
    { odoc_input
    ; html_dir
    ; html_file = html_dir ++ "index.html"
    ; typ = `Module
    ; html_alias
    }
  | Pkg _ ->
    { odoc_input
    ; html_dir = html_base
    ; html_file = html_base ++ sprintf "%s.html" (
        Path.basename odoc_input
        |> Filename.chop_extension
        |> String.drop_prefix ~prefix:"page-"
        |> Option.value_exn
      )
    ; typ = `Mld
    ; html_alias
    }

let setup_pkg_html_rules =
  let loaded = Package.Name.Table.create ~default_value:None in
  let odoc_glob =
    Re.compile (Re.seq [Re.(rep1 any) ; Re.str ".odoc" ; Re.eos]) in
  fun sctx ~pkg ~libs ->
    if Package.Name.Table.get loaded pkg = None then begin
      Package.Name.Table.set loaded ~key:pkg ~data:(Some ());
      let odocs =
        let odocs dir =
          SC.eval_glob sctx ~dir odoc_glob
          |> List.map ~f:(Path.relative dir)
        in
        List.concat (
          (List.map ~f:(fun odoc_input ->
             create_odoc sctx (Pkg pkg) ~odoc_input
           ) (odocs (Paths.odocs sctx (Pkg pkg))))
          :: (
            List.map libs ~f:(fun lib ->
              odocs (Paths.odocs sctx (Lib lib))
              |> List.map ~f:(fun odoc_input ->
                create_odoc sctx (Lib lib) ~odoc_input
              )
            )
          )) in
      let odoc = get_odoc sctx in
      let html_files =
        let closure =
          match Lib.closure libs with
          | Ok closure -> closure
          | Error _ -> libs in
        let deps = Build.return closure >>> Dep.deps sctx in
        List.map odocs ~f:(fun odoc_file ->
          to_html sctx odoc_file ~odoc ~deps) in
      let html_root = Paths.html_root sctx in
      List.iter (
        Dep.html_alias sctx (Pkg pkg)
        :: List.map ~f:(fun lib -> Dep.html_alias sctx (Lib lib)) libs
      ) ~f:(fun alias ->
        SC.add_alias_deps sctx alias
          [ css_file ~doc_dir:html_root
          ; toplevel_index ~doc_dir:html_root
          ]
      );
      List.combine odocs html_files
      |> List.iter ~f:(fun (odoc, html) ->
        SC.add_alias_deps sctx odoc.html_alias [html]
      );
    end

let gen_rules sctx ~dir:_ rest =
  match rest with
  | ["_html"] ->
    setup_css_rule sctx;
    setup_toplevel_index_rule sctx
  | "_mlds" :: _pkg :: _
  | "_odoc" :: "pkg" :: _pkg :: _ ->
    () (* rules were already setup lazily in gen_rules *)
  | "_odoc" :: "lib" :: lib :: _ ->
    let lib, lib_db = read_unique_name sctx lib in
    begin match Lib.DB.find lib_db lib with
    | Error _ -> ()
    | Ok lib  -> SC.load_dir sctx ~dir:(Lib.src_dir lib)
    end
  | "_html" :: lib_unique_name_or_pkg :: _ ->
    (* TODO we can be a better with the error handling in the case where
       lib_unique_name_or_pkg is neither a valid or pkg or lnu *)
    let lib, lib_db = read_unique_name sctx lib_unique_name_or_pkg in
    begin match Lib.DB.find lib_db lib with
    | Error _ -> ()
    | Ok lib  ->
      Option.iter (Lib.pkg lib) ~f:(fun pkg ->
        let libs =
          Lib.Set.to_list (load_all_odoc_rules_pkg sctx ~pkg ~lib_db) in
        setup_pkg_html_rules sctx ~pkg ~libs
      )
    end;
    Option.iter
      (Package.Name.Map.find (SC.packages sctx)
         (Package.Name.of_string lib_unique_name_or_pkg))
      ~f:(fun pkg ->
        let lib_db = db_of_pkg sctx ~pkg in
        let libs =
          Lib.Set.to_list (load_all_odoc_rules_pkg sctx ~pkg:pkg.name ~lib_db)
        in
        setup_pkg_html_rules sctx ~pkg:pkg.name ~libs;
      )
  | _ -> ()

let setup_package_aliases sctx (pkg : Package.t) =
  let alias = html_alias sctx pkg in
  SC.add_alias_deps sctx alias (
    Dep.html_alias sctx (Pkg pkg.name)
    :: (libs_of_pkg ~pkg:pkg.name (db_of_pkg sctx ~pkg)
        |> Lib.Set.to_list
        |> List.map ~f:(fun lib -> Dep.html_alias sctx (Lib lib)))
    |> List.map ~f:Build_system.Alias.stamp_file
  )

let pkg_odoc sctx (pkg : Package.t) = Paths.odocs sctx (Pkg pkg.name)

let entry_modules sctx ~(pkg : Package.t) ~entry_modules_by_lib =
  let lib_db = SC.public_libs sctx in
  libs_of_pkg lib_db ~pkg:pkg.name
  |> Lib.Set.to_list
  |> List.filter_map ~f:(fun l ->
    if Lib.is_local l then (
      Some (l, entry_modules_by_lib l)
    ) else (
      None
    ))
  |> Lib.Map.of_list_exn

let default_index entry_modules =
  let b = Buffer.create 512 in
  Lib.Map.iteri entry_modules ~f:(fun lib modules ->
    Buffer.add_string b (
      sprintf
        "{1 Library %s}\n\
         This library exposes the following toplevel modules: {!modules:%s}.\n"
        (Lib.name lib)
        (modules
         |> List.map ~f:(fun m -> Module.Name.to_string (Module.name m))
         |> String.concat ~sep:" ")
    )
  );
  Buffer.contents b

let check_mlds_no_dupes ~pkg ~mlds =
  match
    List.map mlds ~f:(fun mld -> (Path.basename mld, mld))
    |> String_map.of_list
  with
  | Ok m -> m
  | Error (_, p1, p2) ->
    die "Package %s has two mld's with the same basename %s, %s"
      (Package.Name.to_string pkg.Package.name)
      (Path.to_string_maybe_quoted p1)
      (Path.to_string_maybe_quoted p2)

let setup_package_odoc_rules sctx ~pkg ~mlds ~entry_modules_by_lib =
  let mlds = check_mlds_no_dupes ~pkg ~mlds in
  let mlds =
    if String_map.mem mlds "index" then
      mlds
    else
      let entry_modules = entry_modules sctx ~pkg ~entry_modules_by_lib in
      let gen_mld = Paths.gen_mld_dir sctx pkg ++ "index.mld" in
      SC.add_rule sctx (
        Build.write_file gen_mld (default_index entry_modules)
      );
      String_map.add mlds "index" gen_mld in
  let odoc = get_odoc sctx in
  let odocs = List.map (String_map.values mlds) ~f:(fun mld ->
    compile_mld
      sctx
      (Mld.create mld)
      ~pkg:pkg.name
      ~odoc
      ~doc_dir:(Paths.odocs sctx (Pkg pkg.name))
      ~includes:(Build.arr (fun _ -> Arg_spec.As []))
  ) in
  Dep.setup_deps sctx (Pkg pkg.name) odocs
