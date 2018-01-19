open Import
open Jbuild
open Build.O
open! No_io

type rule =
  { exe: Jbuild.Executables.t
  ; alias_name: string
  ; alias_action: (unit, Action.t) Build.t
  ; alias_stamp: Sexp.t
  ; all_modules: Module.t String_map.t
  ; gen_source : (unit, Action.t) Build.t
  }

module Ppx_info = struct
  type t =
    { uses_inline_test: bool
    ; uses_expect: bool
    }

  let of_libs libs =
    let uses_expect = ref false in
    let uses_inline_test = ref false in
    List.iter libs ~f:(fun lib ->
      match Lib.best_name lib with
      | "ppx_inline_test" -> uses_inline_test := true
      | "ppx_expect" -> uses_expect := true
      | _ -> ()
    );
    { uses_expect = !uses_expect
    ; uses_inline_test = !uses_inline_test
    }
end

let setup_rules ppx_info ~sctx ~dir ~(lib : Jbuild.Library.t)
      ~(inline_tests : Jbuild.Inline_tests.t) ~scope =
  let name = lib.name ^ "_test_runner" in
  let module_filename = name ^ ".ml-gen" in
  let module_name = String.capitalize_ascii name in
  let exe_stanza =
    { Jbuild.Executables.names = [name]
    ; link_executables = true
    ; link_flags = Ordered_set_lang.Unexpanded.t (
        Sexp.add_loc ~loc:Loc.none (List [Atom "-linkall"])
      )
    ; modes = Mode.Dict.Set.all
    ; buildable =
        { Buildable.modules = Ordered_set_lang.t (
            Sexp.add_loc ~loc:Loc.none (List [Atom module_name])
          )
        ; libraries =
            List.map ~f:Lib_dep.direct (
              [lib.name]
              @ (if ppx_info.Ppx_info.uses_expect then
                   ["ppx_expect.evaluator"]
                 else
                   [])
              @ ["ppx_inline_test.runner.lib"]
          )
        ; preprocess = Preprocess_map.no_preprocessing
        ; preprocessor_deps = []
        ; flags = Ordered_set_lang.Unexpanded.standard
        ; ocamlc_flags = Ordered_set_lang.Unexpanded.standard
        ; ocamlopt_flags = Ordered_set_lang.Unexpanded.standard
        ; js_of_ocaml = Js_of_ocaml.default
        ; gen_dot_merlin = false
        ; lint = Jbuild.Lint.no_lint
        }
    } in
  { exe = exe_stanza
  ; alias_name = "runtest"
  ; alias_stamp = Sexp.List [Atom "ppx-runner"; Atom name]
  ; alias_action =
      (let module A = Action in
       let exe = Path.relative dir (name ^ ".exe") in
       Build.path exe >>>
       Super_context.Deps.interpret sctx ~scope ~dir inline_tests.deps
       >>^ fun _ ->
       A.chdir dir
         (A.run (Ok exe) ["inline-test-runner"; lib.name]))
  ; gen_source = (
      let pps = Jbuild.Preprocess_map.pps lib.buildable.preprocess in
      let pp_requires = Super_context.PP.get_ppx_driver_requires sctx pps in
      pp_requires >>^ (fun libs ->
        List.iter ~f:(fun lib ->
          Format.eprintf "checking lib: %s@." (Lib.best_name lib)
        )
      )
      >>^ ignore
      >>>
      Build.write_file (Path.relative dir module_filename)
        "let () = Ppx_inline_test_lib.Runtime.exit ()"
    )
  ; all_modules =
      (String_map.of_alist_exn
         [ module_name
         , { Module.
             name = module_name
           ; impl = { Module.File.
                      name = module_filename
                    ; syntax = Module.Syntax.OCaml
                    }
           ; intf = None
           ; obj_name = "" } ])
  }
;;

let rule sctx ~(inline_tests : Jbuild.Inline_tests.t) ~(lib : Jbuild.Library.t)
      ~dir ~scope =
  setup_rules ~sctx ~dir ~inline_tests ~lib ~scope
