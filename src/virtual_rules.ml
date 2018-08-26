open! Stdune
open Dune_file

module Implementation = struct
  type t =
    { vlib            : Lib.t
    ; impl            : Dune_file.Library.t
    ; virtual_modules : Module.t Module.Name.Map.t
    ; vlib_modules    : Module.t Module.Name.Map.t
    ; ext_obj         : string
    }

  let vlib_stubs_o_files t ~dir =
    let obj_dir = Utils.library_object_directory ~dir t.impl.name in
    Module.Name.Map.fold t.virtual_modules ~init:[] ~f:(fun m acc ->
      if Module.has_impl m then
        Module.obj_file m ~obj_dir ~ext:t.ext_obj :: acc
      else
        acc)

  let dep_graph
        { vlib ; vlib_modules = modules; impl = _
        ; virtual_modules = _ ; ext_obj = _ }
        (impl_graph : Ocamldep.Dep_graphs.t) =
    let obj_dir = Lib.obj_dir vlib in
    let vlib_graph = Ocamldep.rules_for_lib ~obj_dir ~modules in
    { Ml_kind.Dict.
      impl = impl_graph.impl (* TODO *)
    (* implementations don't introduce interface deps *)
    ; intf = vlib_graph.intf
    }

  let add_vlib_modules t s =
    Module.Name.Map.superpose t.virtual_modules s
end

module Gen (S : sig val sctx : Super_context.t end) = struct
  open S

  let ctx = Super_context.context sctx

  let setup_copy_rules_for_impl ~dir
        { Implementation.
          impl
        ; vlib
        ; vlib_modules
        ; virtual_modules = _
        ; ext_obj = _
        } =
    let copy_to_obj_dir =
      let obj_dir = Utils.library_object_directory ~dir impl.name in
      fun file ->
        let dst = Path.relative obj_dir (Path.basename file) in
        Super_context.add_rule sctx (Build.symlink ~src:file ~dst)
    in
    let obj_dir = Lib.obj_dir vlib in
    let modes =
      Mode_conf.Set.eval impl.modes
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

  let implements_rules ~(lib : Library.t) ~scope ~modules (loc, implements) =
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
        | Some (Expanded virtual_modules) ->
          (* TODO make this work for external libraries *)
          (Module.Name.Map.empty, virtual_modules)
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
      ; virtual_modules
      ; vlib_modules
      ; ext_obj = ctx.ext_obj
      }
end
