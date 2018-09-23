open Import
open! No_io

module Implementation = struct
  type t =
    { vlib            : Lib.t
    ; impl            : Dune_file.Library.t
    ; vlib_modules    : Lib_modules.t
    }

  let modules_of_vlib t = Lib_modules.source_modules t.vlib_modules

  let dep_graph ({ vlib ; vlib_modules = _ ; impl = _ } as t)
        (impl_graph : Ocamldep.Dep_graphs.t) =
    let modules_of_vlib = modules_of_vlib t in
    let obj_dir = Lib.obj_dir vlib in
    let vlib_graph =
      Ocamldep.graph_of_remote_lib ~obj_dir ~modules:modules_of_vlib in
    Ocamldep.Dep_graphs.merge_for_impl ~vlib:vlib_graph ~impl:impl_graph

end

module Gen (P : sig val sctx : Super_context.t end) = struct
  open P
  let ctx = Super_context.context sctx

  let vlib_stubs_o_files { Implementation.vlib ; _ } =
    Lib.foreign_objects vlib

  let setup_copy_rules_for_impl ~dir
        ({ Implementation.vlib ; impl ; vlib_modules } as t) =
    let modules_of_vlib =
      let modules_of_vlib = Implementation.modules_of_vlib t in
      match Lib_modules.alias vlib_modules with
      | None -> modules_of_vlib
      | Some { alias_module; module_name = _ } ->
        Module.Name.Map.add modules_of_vlib
          (Module.name alias_module) alias_module
    in
    let copy_to_obj_dir =
      let obj_dir = Utils.library_object_directory ~dir (snd impl.name) in
      fun file ->
        let dst = Path.relative obj_dir (Path.basename file) in
        Super_context.add_rule ~loc:(Loc.of_pos __POS__)
          sctx (Build.symlink ~src:file ~dst)
    in
    let obj_dir = Lib.obj_dir vlib in
    let modes =
      Dune_file.Mode_conf.Set.eval impl.modes
        ~has_native:(Option.is_some ctx.ocamlopt) in
    Module.Name.Map.iter modules_of_vlib ~f:(fun m ->
      let copy_obj_file ext =
        copy_to_obj_dir (Module.obj_file m ~obj_dir ~ext) in
      copy_obj_file (Cm_kind.ext Cmi);
      if Module.has_intf m then begin
        Module.file m Ml_kind.Intf
        |> Option.value_exn
        |> Path.extend_basename ~suffix:".all-deps"
        |> copy_to_obj_dir
      end;
      if Module.has_impl m then begin
        if modes.byte then
          copy_obj_file (Cm_kind.ext Cmo);
        if modes.native then
          List.iter [Cm_kind.ext Cmx; ctx.ext_obj] ~f:copy_obj_file
      end)

  let module_list ms =
    List.map ms ~f:(fun m -> sprintf "- %s" (Module.Name.to_string m))
    |> String.concat ~sep:"\n"

  let check_module_fields ~(lib : Dune_file.Library.t) ~virtual_modules
        ~source_modules ~implements =
    let new_public_modules =
      Module.Name.Map.foldi source_modules ~init:[] ~f:(fun name m acc ->
        if Module.is_public m
        && not (Module.Name.Map.mem virtual_modules name) then
          name :: acc
        else
          acc)
    in
    if new_public_modules <> [] then begin
      Errors.fail lib.buildable.loc
        "The following modules aren't part of the virtual library's interface:\
         \n%s\n\
         They must be marked as private using the (private_modules ..) field"
        (module_list new_public_modules)
    end;
    let (missing_modules, impl_modules_with_intf, private_virtual_modules) =
      Module.Name.Map.foldi virtual_modules ~init:([], [], [])
        ~f:(fun m _ (mms, ims, pvms) ->
          match Module.Name.Map.find source_modules m with
          | None -> (m :: mms, ims, pvms)
          | Some m ->
            let ims =
              if Module.has_intf m then
                Module.name m :: ims
              else
                ims
            in
            let pvms =
              if Module.is_public m then
                pvms
              else
                Module.name m :: pvms
            in
            (mms, ims, pvms))
    in
    if private_virtual_modules <> [] then begin
      (* The loc here will never be none as we've some private modules *)
      Errors.fail_opt (Option.bind lib.private_modules ~f:Ordered_set_lang.loc)
        "These private modules cannot be private:\n%s"
        (module_list private_virtual_modules)
    end;
    if missing_modules <> [] then begin
      Errors.fail lib.buildable.loc
        "Library %a cannot implement %a because the following \
         modules lack an implementation:\n%s"
        Lib_name.Local.pp (snd lib.name)
        Lib_name.pp implements
        (module_list missing_modules)
    end;
    if impl_modules_with_intf <> [] then begin
      Errors.fail lib.buildable.loc
        "The following modules cannot have .mli files as they implement \
         virtual modules:\n%s"
        (module_list impl_modules_with_intf)
    end

  let impl ~(lib : Dune_file.Library.t) ~scope ~source_modules =
    Option.map lib.implements ~f:begin fun (loc, implements) ->
      match Lib.DB.find (Scope.libs scope) implements with
      | Error _ ->
        Errors.fail loc
          "Cannot implement %a as that library isn't available"
          Lib_name.pp implements
      | Ok vlib ->
        let virtual_modules =
          Option.map (Lib.virtual_ vlib) ~f:(fun (v : Lib_info.Virtual.t) ->
            v.modules)
        in
        let vlib_modules =
          match virtual_modules with
          | None ->
            Errors.fail lib.buildable.loc
              "Library %a isn't virtual and cannot be implemented"
              Lib_name.pp implements
          | Some Unexpanded ->
            let dir_contents =
              Dir_contents.get sctx ~dir:(Lib.src_dir vlib) in
            Dir_contents.modules_of_library dir_contents
              ~name:(Lib.name vlib)
        in
        let virtual_modules = Lib_modules.virtual_modules vlib_modules in
        check_module_fields ~lib ~virtual_modules ~source_modules ~implements;
        { Implementation.
          impl = lib
        ; vlib
        ; vlib_modules
        }
    end
end
