open Import
open Jbuild
open Build.O
open! No_io

let exe_name = "utop"
let module_name = String.capitalize_ascii exe_name
let module_filename = exe_name ^ ".ml"

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
  let path = Path.relative dir module_filename in
  let utop_ml =
    lib_requires
    >>^ (fun libs ->
      let include_paths =
        let ctx = Super_context.context sctx in
        Path.Set.elements
          (Lib.L.include_paths libs ~stdlib_dir:ctx.stdlib_dir)
      in
      let b = Buffer.create 64 in
      let fmt = Format.formatter_of_buffer b in
      pp_ml fmt include_paths;
      Format.pp_print_flush fmt ();
      Buffer.contents b)
    >>> Build.write_file_dyn path in
  Super_context.add_rule sctx utop_ml

let utop_of_libs (libs : Library.t list) =
  { Executables.names = [(Loc.none, exe_name)]
  ; link_executables = true
  ; link_flags = Ordered_set_lang.Unexpanded.t (
      Sexp.add_loc ~loc:Loc.none
        (List [ Atom "-linkall"
              ; Atom "-warn-error"
              ; Atom "-31" ])
    )
  ; modes = Mode.Dict.Set.of_list [Mode.Byte]
  ; buildable =
      { Buildable.
        loc = Loc.none
      ; modules =
          Ordered_set_lang.t (List (Loc.none, [Atom (Loc.none, module_name)]))
      ; modules_without_implementation = Ordered_set_lang.standard
      ; libraries =
          (Lib_dep.direct "utop") :: (List.map libs ~f:(fun lib ->
            Lib_dep.direct lib.Library.name))
      ; preprocess = Preprocess_map.no_preprocessing
      ; lint = Lint.no_lint
      ; preprocessor_deps = []
      ; flags = Ordered_set_lang.Unexpanded.standard
      ; ocamlc_flags = Ordered_set_lang.Unexpanded.standard
      ; ocamlopt_flags = Ordered_set_lang.Unexpanded.standard
      ; js_of_ocaml = Js_of_ocaml.default
      ; gen_dot_merlin = false
      }
  }

let exe_stanzas stanzas =
  let libs =
    List.filter_map stanzas ~f:(function
      | Stanza.Library lib -> Some lib
      | _ -> None
    ) in
  match libs with
  | [] -> None
  | libs ->
    let all_modules =
      String_map.of_alist_exn
        [ module_name
        , { Module.
            name = module_name
          ; impl = Some { Module.File.
                          name = module_filename
                        ; syntax = Module.Syntax.OCaml
                        }
          ; intf = None
          ; obj_name = "" }
        ] in
    Some (utop_of_libs libs, all_modules)

let utop_exe_dir ~dir = Path.relative dir ".utop"

let utop_exe dir =
  Path.relative (utop_exe_dir ~dir) exe_name
  (* Use the [.exe] version. As the utop executable is declared with
     [(modes (byte))], the [.exe] correspond the bytecode linked in
     custom mode. We do that so that it works without hassle when
     generating a utop for a library with C stubs. *)
  |> Path.extend_basename ~suffix:(Mode.exe_ext Mode.Native)
