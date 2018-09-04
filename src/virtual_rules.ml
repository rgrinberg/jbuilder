open Import
open! No_io

module Implementation = struct
  type t =
    { vlib            : Lib.t
    ; impl            : Dune_file.Library.t
    ; vlib_modules    : Module.t Module.Name.Map.t
    }
end

module Gen (P : sig val sctx : Super_context.t end) = struct
  open P
  let ctx = Super_context.context sctx

  let setup_copy_rules_for_impl ~dir
        { Implementation.vlib ; impl ; vlib_modules } =
    let copy_to_obj_dir =
      let obj_dir = Utils.library_object_directory ~dir impl.name in
      fun file ->
        let dst = Path.relative obj_dir (Path.basename file) in
        Super_context.add_rule sctx (Build.symlink ~src:file ~dst)
    in
    let obj_dir = Lib.obj_dir vlib in
    let modes =
      Dune_file.Mode_conf.Set.eval impl.modes
        ~has_native:(Option.is_some ctx.ocamlopt) in
    Module.Name.Map.iter vlib_modules ~f:(fun m ->
      let copy_obj_file ext =
        copy_to_obj_dir (Module.obj_file m ~obj_dir ~ext) in
      copy_obj_file (Cm_kind.ext Cmi);
      if Module.has_impl m then begin
        if modes.byte then
          copy_obj_file (Cm_kind.ext Cmo);
        if modes.native then
          List.iter [Cm_kind.ext Cmx; ctx.ext_obj] ~f:copy_obj_file
      end)

  let impl ~(lib : Dune_file.Library.t) ~scope ~modules =
    Option.map lib.implements ~f:begin fun (loc, implements) ->
      match Lib.DB.find (Scope.libs scope) implements with
      | Error _ ->
        Errors.fail loc
          "Cannot implement %s as that library isn't available"
          implements
      | Ok vlib ->
        let (vlib_modules, virtual_modules) =
          match Lib.virtual_modules vlib with
          | None ->
            Errors.fail lib.buildable.loc
              "Library %s isn't virtual and cannot be implemented"
              implements
          | Some Unexpanded ->
            let dir_contents = Dir_contents.get sctx ~dir:(Lib.src_dir vlib) in
            let { Dir_contents.Library_modules.
                  virtual_modules
                ; modules = vlib_modules
                ; main_module_name = _
                ; alias_module = _
                } =
              Dir_contents.modules_of_library dir_contents
                ~name:(Lib.name vlib) in
            (vlib_modules, virtual_modules)
        in
        let (missing_modules, impl_modules_with_intf) =
          Module.Name.Map.foldi virtual_modules ~init:([], [])
            ~f:(fun m _ (mms, ims) ->
              match Module.Name.Map.find modules m with
              | None -> (m :: mms, ims)
              | Some m ->
                if Module.has_intf m then
                  (mms, Module.name m :: ims)
                else
                  (mms, ims))
        in
        let module_list ms =
          List.map ms ~f:Module.Name.to_string
          |> String.concat ~sep:"\n"
        in
        if missing_modules <> [] then begin
          Errors.fail lib.buildable.loc
            "Library %s cannot implement %s because the following \
             modules lack an implementation:\n%s"
            lib.name implements
            (module_list missing_modules)
        end;
        if impl_modules_with_intf <> [] then begin
          Errors.fail lib.buildable.loc
            "The following modules cannot have .mli files as they implement \
             virtual modules:\n%s"
            (module_list impl_modules_with_intf)
        end;
        { Implementation.
          impl = lib
        ; vlib
        ; vlib_modules
        }
    end
end
