open! Stdune
open Dune_file

module Gen (S : sig val sctx : Super_context.t end) = struct
  open S

  let implements_rules ~(lib : Library.t) ~scope ~modules (loc, implements) =
    match Lib.DB.find (Scope.libs scope) implements with
    | Error _ ->
      Errors.fail loc
        "Cannot implement %s as that library isn't available"
        implements
    | Ok l ->
      let virtual_modules =
        match Lib.virtual_modules l with
        | None ->
          Errors.fail lib.buildable.loc
            "Library %s isn't virtual and cannot be implemented"
            implements
        | Some Unexpanded ->
          let dir_contents = Dir_contents.get sctx ~dir:(Lib.src_dir l) in
          let { Dir_contents.Library_modules.
                virtual_modules ; _ } =
            Dir_contents.modules_of_library dir_contents
              ~name:(Lib.name l) in
          Module.Name.Map.keys virtual_modules
        | Some Expanded virtual_modules ->
          virtual_modules
      in
      let (missing_modules, impl_modules_with_intf) =
        List.fold_left virtual_modules ~init:([], [])
          ~f:(fun (mms, ims) m ->
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
      end

  let setup_copy_rules_for_impl ~dir:_ ~impl:_ ~vlib:_ =
    ()
end
