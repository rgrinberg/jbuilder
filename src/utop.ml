open Import
open Jbuild
open Build.O
open! No_io

let exe_name = "_utop"
let main_module_name = "Utop"
let main_module_filename = exe_name ^ ".ml"

let pp_ml fmt include_dirs =
  let pp_include fmt =
    let pp_sep fmt () = Format.fprintf fmt "@ ; " in
    Format.pp_print_list ~pp_sep (fun fmt p ->
      Format.fprintf fmt "%S" (Path.to_absolute_filename p)
    ) fmt
  in
  Format.fprintf fmt "@[<v 2>Clflags.include_dirs :=@ [ %a@ ]@];@."
    pp_include include_dirs;
  Format.fprintf fmt "@.UTop_main.main ();@."

let add_module_rules sctx ~dir lib_requires =
  let path = Path.relative dir main_module_filename in
  let utop_ml =
    lib_requires
    >>^ (fun libs ->
      let include_paths =
        let ctx = Super_context.context sctx in
        Path.Set.to_list
          (Lib.L.include_paths libs ~stdlib_dir:ctx.stdlib_dir)
      in
      let b = Buffer.create 64 in
      let fmt = Format.formatter_of_buffer b in
      pp_ml fmt include_paths;
      Format.pp_print_flush fmt ();
      Buffer.contents b)
    >>> Build.write_file_dyn path in
  Super_context.add_rule sctx utop_ml

let utop_exe dir =
  Path.relative dir exe_name
  (* Use the [.exe] version. As the utop executable is declared with
     [(modes (byte))], the [.exe] correspond the bytecode linked in
     custom mode. We do that so that it works without hassle when
     generating a utop for a library with C stubs. *)
  |> Path.extend_basename ~suffix:(Mode.exe_ext Mode.Native)

let setup sctx ~dir ~(libs : Library.t list) ~scope =
  match libs with
  | [] -> ()
  | lib::_ ->
    let loc = lib.buildable.loc in
    let modules =
      String_map.singleton
        main_module_name
        { Module.
          name = main_module_name
        ; impl = Some { Module.File.
                        name = main_module_filename
                      ; syntax = Module.Syntax.OCaml
                      }
        ; intf = None
        ; obj_name = "" } in
    let requires, _ =
      Lib.DB.find_many (Scope.libs scope)
        ("utop" :: List.map libs ~f:(fun (lib : Library.t) -> lib.name))
        ~required_by:[]
      |> Lib.Compile.make
      |> Super_context.Libs.requires sctx ~loc ~dir
           ~has_dot_merlin:false
    in
    let (_obj_dir : Path.t) =
      Exe.build_and_link sctx
        ~dir
        ~program:{ name = exe_name ; main_module_name }
        ~modules
        ~scope
        ~linkages:[Exe.Linkage.custom]
        ~requires
        ~flags:(Ocaml_flags.append_common (Ocaml_flags.default ()) ["-w"; "-24"])
        ~link_flags:(Build.return ["-linkall"; "-warn-error"; "-31"])
    in
    add_module_rules sctx ~dir requires
