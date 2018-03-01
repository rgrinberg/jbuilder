open Import
open Jbuild
open Build.O

module SC = Super_context

let lib_unique_name lib =
  let name = Lib.name lib in
  match Lib.status lib with
  | Installed -> assert false
  | Public    -> name
  | Private scope_name ->
    sprintf "%s@%s" name (Scope_info.Name.to_string scope_name)

module Doc = struct
  let root sctx = Path.relative (SC.context sctx).Context.build_dir "_doc"

  let alias = Build_system.Alias.make ".odoc-all"

  let odoc_lib_dir sctx ~lib =
    Path.relative (root sctx) (sprintf "_odoc/lib/%s" (lib_unique_name lib))

  let deps t =
    Build.dyn_paths (Build.arr (
      List.fold_left ~init:[] ~f:(fun acc (lib : Lib.t) ->
        if Lib.is_local lib then (
          let dir = odoc_lib_dir t ~lib in
          Build_system.Alias.stamp_file (alias ~dir) :: acc
        ) else (
          acc
        )
      )))

  let alias t lib = alias ~dir:(odoc_lib_dir t ~lib)

  (* let static_deps t lib = Build_system.Alias.dep (alias t lib) *)

  let setup_deps t lib files = SC.add_alias_deps t (alias t lib) files

  let odoc_pkg_dir sctx ~pkg =
    Path.relative (root sctx) (sprintf "_odoc/pkg/%s" pkg)

  let pkg_html sctx ~pkg =
    Path.relative (root sctx) (sprintf "_html/_pkg/%s" pkg)

  let lib_html sctx ~lib =
    Path.relative (root sctx) (sprintf "_html/%s" (lib_unique_name lib))

  module L = struct
    let odoc_include_flags sctx libs =
      let paths =
        libs |> List.fold_left ~f:(fun paths lib ->
          if Lib.is_local lib then (
            Path.Set.add paths (odoc_lib_dir sctx ~lib)
          ) else (
            paths
          )
        ) ~init:Path.Set.empty in
      Arg_spec.S (List.concat_map (Path.Set.to_list paths)
                    ~f:(fun dir -> [Arg_spec.A "-I"; Path dir]))
  end
end


let ( ++ ) = Path.relative

let get_odoc sctx = SC.resolve_program sctx "odoc" ~hint:"opam install odoc"
let odoc_ext = ".odoc"

module Mld : sig
  type t

  val odoc_file : doc_dir:Path.t -> t -> Path.t
  val odoc_input : doc_dir:Path.t -> t -> Path.t

end = struct
  type t = string (** source file name without the extension. *)

  let odoc_file ~doc_dir t =
    Path.relative doc_dir (sprintf "page-%s%s" t odoc_ext)

  let odoc_input ~doc_dir t =
    Path.relative doc_dir (sprintf "%s-generated.mld" t)

end

module Module_or_mld = struct
  [@@@warning "-37"]
  type t =
    | Mld of Mld.t
    | Module of Module.t

  let odoc_file ~doc_dir = function
    | Mld m -> Mld.odoc_file ~doc_dir m
    | Module m -> Module.odoc_file ~doc_dir m

  let odoc_input ~obj_dir ~doc_dir = function
    | Mld m -> Mld.odoc_input ~doc_dir m
    | Module m -> Module.cmti_file m ~obj_dir
end

let module_or_mld_deps (m : Module_or_mld.t) ~doc_dir
      ~(dep_graphs:Ocamldep.Dep_graphs.t) =
  match m with
  | Mld _ ->
    Build.arr (fun x -> x)
  | Module m ->
    Build.dyn_paths
      ((match m.intf with
         | Some _ ->
           Ocamldep.Dep_graph.deps_of dep_graphs.intf m
         | None ->
           (* When a module has no .mli, use the dependencies for the .ml *)
           Ocamldep.Dep_graph.deps_of dep_graphs.impl m)
       >>^ List.map ~f:(Module.odoc_file ~doc_dir))

let compile sctx (m : Module_or_mld.t) ~odoc ~dir ~obj_dir ~includes ~dep_graphs
      ~doc_dir ~pkg =
  let context = SC.context sctx in
  let odoc_file = Module_or_mld.odoc_file m ~doc_dir in
  SC.add_rule sctx
    (module_or_mld_deps m ~doc_dir ~dep_graphs
     >>>
     includes
     >>>
     Build.run ~context ~dir:doc_dir odoc
       [ A "compile"
       ; A "-I"; Path dir
       ; Dyn (fun x -> x)
       ; As ["--pkg"; pkg]
       ; A "-o"; Target odoc_file
       ; Dep (Module_or_mld.odoc_input m ~obj_dir ~doc_dir)
       ]);
  (m, odoc_file)

type odoc =
  { odoc_input: Path.t
  ; html_dir: Path.t
  ; html_file: Path.t
  ; typ: [`Module | `Mld]
  }

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
  Format.eprintf "Generating rule for: %a@.%!" Path.pp odoc_file.html_file;
  let html_output_dir = Doc.root sctx ++ "_html" in
  SC.add_rule sctx
    (deps
     >>> Build.progn (
       Build.remove_tree to_remove
       :: Build.mkdir odoc_file.html_dir
       :: Build.run ~context ~dir:html_output_dir
            odoc ~extra_targets:[odoc_file.html_file]
            [ A "html"
            ; Dyn (Doc.L.odoc_include_flags sctx)
            ; A "-o"; Path html_output_dir
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
  let lib_unique_name = lib_unique_name lib in
    let pkg = Option.value (Lib.pkg lib) ~default:lib_unique_name in
    let doc_dir = Doc.odoc_lib_dir sctx ~lib in
    let obj_dir = Lib.obj_dir lib in
    let odoc = get_odoc sctx in
    let includes =
      let ctx = SC.context sctx in
      Build.memoize "includes"
        (requires
         >>> Doc.deps sctx
         >>^ Lib.L.include_flags ~stdlib_dir:ctx.stdlib_dir)
    in
    let modules_and_odoc_files =
      List.map (String_map.values modules) ~f:(fun m ->
        compile sctx ~odoc ~dir ~obj_dir ~includes ~dep_graphs
          ~doc_dir ~pkg (Module m))
    in
    Doc.setup_deps sctx lib (List.map modules_and_odoc_files ~f:snd)

 (* 
 * let setup_library_rules sctx (lib : Library.t) ~dir ~scope ~modules ~mld_files
 *       ~requires ~(dep_graphs:Ocamldep.Dep_graph.t Ml_kind.Dict.t) =
 *   let doc_dir = Doc.dir sctx lib in
 *   let obj_dir, lib_unique_name =
 *     let lib =
 *       Option.value_exn (Lib.DB.find_even_when_hidden (Scope.libs scope) lib.name)
 *     in
 *     let name =
 *       let name = Lib.name lib in
 *       match Lib.status lib with
 *       | Installed -> assert false
 *       | Public    -> name
 *       | Private scope_name ->
 *         sprintf "%s@%s" name (Scope_info.Name.to_string scope_name)
 *     in
 *     (Lib.obj_dir lib, name)
 *   in
 *   let odoc = get_odoc sctx in
 *   let includes =
 *     let ctx = SC.context sctx in
 *     Build.memoize "includes"
 *       (requires
 *        >>> Doc.deps sctx
 *        >>^ Lib.L.include_flags ~stdlib_dir:ctx.stdlib_dir)
 *   in
 *   let mld_files =
 *     all_mld_files sctx ~dir ~lib ~modules mld_files
 *   in
 *   let mld_and_odoc_files =
 *     List.map mld_files ~f:(fun m ->
 *       compile sctx ~odoc ~dir ~obj_dir ~includes ~dep_graphs
 *         ~doc_dir ~pkg:lib_unique_name (Mld m))
 *   in
 *   let modules_and_odoc_files =
 *     List.map (String_map.values modules) ~f:(fun m ->
 *       compile sctx ~odoc ~dir ~obj_dir ~includes ~dep_graphs
 *         ~doc_dir ~pkg:lib_unique_name (Module m))
 *   in
 *   let inputs_and_odoc_files = modules_and_odoc_files @ mld_and_odoc_files in
 *   Doc.setup_deps sctx lib (List.map inputs_and_odoc_files ~f:snd);
 *   (*
 *      let modules_and_odoc_files =
 *      if lib.wrapped then
 *      let main_module_name = String.capitalize lib.name in
 *      List.filter modules_and_odoc_files
 *      ~f:(fun (m, _) -> m.Module.name = main_module_name)
 *      else
 *      modules_and_odoc_files
 *      in*)
 *   let html_files =
 *     List.map inputs_and_odoc_files ~f:(fun odoc_file ->
 *       to_html sctx odoc_file ~doc_dir ~odoc ~dir ~includes)
 *   in
 *   let doc_root = Doc.root sctx in
 *   let alias =
 *     match lib.public with
 *     | None -> Build_system.Alias.private_doc ~dir
 *     | Some _ -> Build_system.Alias.doc ~dir in
 *   SC.add_alias_deps sctx alias
 *     (css_file ~doc_dir:doc_root
 *      :: toplevel_index ~doc_dir:doc_root
 *      :: html_files)
 *)

let setup_css_rule sctx =
  let context = SC.context sctx in
  let doc_dir = Doc.root sctx in
  SC.add_rule sctx
    (Build.run ~context
       ~dir:context.build_dir
       ~extra_targets:[css_file ~doc_dir]
       (get_odoc sctx)
       [ A "css"; A "-o"; Path doc_dir ])

let sp = Printf.sprintf

let setup_toplevel_index_rule sctx =
  let list_items =
    Super_context.stanzas_to_consider_for_install sctx
    |> List.filter_map ~f:(fun (_path, _scope, stanza) ->
      match stanza with
      | Stanza.Library
          {Library.kind = Library.Kind.Normal; public = Some public_info; _} ->
        let name = public_info.name in
        let link = sp {|<a href="%s/index.html">%s</a>|} name name in
        let version_suffix =
          match public_info.package.Package.version_from_opam_file with
          | None ->
            ""
          | Some v ->
            sp {| <span class="version">%s</span>|} v
        in
        Some (sp "<li>%s%s</li>" link version_suffix)

      | _ ->
        None)
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
  let doc_dir = Doc.root sctx in
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

let libs_of_pkg ~lib_db ~pkg =
  Lib.DB.all lib_db
  |> List.filter ~f:(fun lib ->
    Lib.is_local lib && (match Lib.pkg lib with
      | None -> false
      | Some pkg' -> pkg' = pkg))

let load_all_odoc_rules_pkg sctx ~lib_db ~pkg =
  let pkg_libs = libs_of_pkg ~lib_db ~pkg in
  if List.length pkg_libs > 7 then (
    List.iter ~f:(fun lib ->
      Format.eprintf "lib: %s@.%!" (Lib.name lib);
      Format.eprintf "src_dir: %a@.%!" Path.pp (Lib.src_dir lib);
        Format.eprintf "status: %s@.%!" (match Lib.status lib with
          | Public -> "public"
          | Private _ -> "Private"
          | Installed -> "installed");
    ) pkg_libs
  );
  SC.load_dir sctx ~dir:(Doc.odoc_pkg_dir sctx ~pkg);
  List.iter pkg_libs ~f:(fun lib ->
    SC.load_dir sctx ~dir:(Doc.odoc_lib_dir sctx ~lib));
  pkg_libs

let create_odoc ~odoc_input ~html_base = function
  | `Module ->
    let html_dir =
      html_base ++ (
        Path.basename odoc_input
        |> Filename.chop_extension
        |> Stdune.String.capitalize_ascii
      ) in
    { odoc_input
    ; html_dir
    ; html_file = html_dir ++ "index.html"
    ; typ = `Module
    }
  | `Mld ->
    { odoc_input
    ; html_dir = html_base
    ; html_file = html_base ++ sprintf "%s.html" (
        Filename.chop_extension (Path.basename odoc_input)
      )
    ; typ = `Mld
    }

let setup_pkg_html_rules =
  let loaded = Hashtbl.create 128 in
  fun sctx ~pkg ~libs ->
    if not (Hashtbl.mem loaded pkg) then begin
      Hashtbl.add loaded pkg ();
      let odocs =
        let glob =
          Re.compile (
            Re.seq [Re.(rep1 any) ; Re.str ".odoc" ; Re.eos]
          ) in
        let odocs dir =
          SC.eval_glob sctx ~dir glob
          |> List.map ~f:(Path.relative dir)
        in
        List.concat (
          (List.map ~f:(fun odoc_input ->
             create_odoc `Mld ~odoc_input ~html_base:(Doc.pkg_html sctx ~pkg)
           ) (odocs (Doc.odoc_pkg_dir sctx ~pkg)))
          :: (
            List.map libs ~f:(fun lib ->
              odocs (Doc.odoc_lib_dir sctx ~lib)
              |> List.map ~f:(fun odoc_input ->
                create_odoc `Module ~odoc_input ~html_base:(
                  Doc.lib_html sctx ~lib
                )
              )
            )
          )) in
      let odoc = get_odoc sctx in
      let html_files =
        let closure =
          match Lib.closure libs with
          | Ok closure -> closure
          | Error _ -> libs in
        let deps = Build.return closure >>> Doc.deps sctx in
        List.map odocs ~f:(fun odoc_file ->
          to_html sctx odoc_file ~odoc ~deps) in
      let doc_root = Doc.root sctx in
      (* let alias =
       *   html_alias sctx (
       *     Option.value_exn (String_map.find (SC.packages sctx) pkg)
       *   ) in *)
      (Doc.pkg_html sctx ~pkg :: List.map ~f:(fun lib -> Doc.lib_html sctx ~lib) libs)
      |> List.iter ~f:(fun dir ->
        let alias = Build_system.Alias.doc ~dir in
        SC.add_alias_deps sctx alias
          (css_file ~doc_dir:doc_root
           :: toplevel_index ~doc_dir:doc_root
           :: html_files)
      )
    end

let gen_rules sctx ~dir:_ rest =
  match rest with
  | [] ->
    setup_css_rule sctx;
    setup_toplevel_index_rule sctx
  | "_odoc" :: "pkg" :: _pkg :: _ ->
    ()
  | "_odoc" :: "lib" :: lib :: _ ->
    let lib, lib_db = read_unique_name sctx lib in
    begin match Lib.DB.find lib_db lib with
    | Error _ -> ()
    | Ok lib  -> SC.load_dir sctx ~dir:(Lib.src_dir lib)
    end
  | "_html" :: "_pkg" :: pkg :: _ ->
    let lib_db =
      Scope_info.Name.of_string pkg
      |> SC.find_scope_by_name sctx
      |> Scope.libs in
    let libs = load_all_odoc_rules_pkg sctx ~pkg ~lib_db in
    setup_pkg_html_rules sctx ~pkg ~libs;
  | "_html" :: lib_unique_name :: _ ->
    let lib, lib_db = read_unique_name sctx lib_unique_name in
    begin match Lib.DB.find lib_db lib with
    | Error _ -> ()
    | Ok lib  ->
      Option.iter (Lib.pkg lib) ~f:(fun pkg ->
        let libs = load_all_odoc_rules_pkg sctx ~pkg ~lib_db in
        setup_pkg_html_rules sctx ~pkg ~libs
      )
    end
  | _ -> failwith "bad path"

let setup_package_aliases sctx (pkg : Package.t) =
  SC.load_dir sctx ~dir:(Doc.pkg_html sctx ~pkg:pkg.name);
  let lib_db =
    Scope_info.Name.of_string pkg.name
    |> SC.find_scope_by_name sctx
    |> Scope.libs in
  let alias = html_alias sctx pkg in
  SC.add_alias_deps sctx alias (
    libs_of_pkg ~lib_db ~pkg:pkg.name|> List.map ~f:(fun lib ->
      let dir = Doc.lib_html sctx ~lib in
      Build_system.Alias.stamp_file (Build_system.Alias.doc ~dir)
    )
  )
