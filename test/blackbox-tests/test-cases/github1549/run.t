Reproduction case for #1549: too many parentheses in installed .dune files

  $ dune build --root backend
  Entering directory 'backend'

  $ cat backend/_build/install/default/lib/dune_inline_tests/dune-package
  (lang dune 1.6)
  (name dune_inline_tests)
  (library
   ((name dune_inline_tests)
    (kind normal)
    (archives ((byte (simple_tests.cma)) (native (simple_tests.cmxa))))
    (plugins ((byte (simple_tests.cma)) (native (simple_tests.cmxs))))
    (foreign_objects ())
    (foreign_archives ((byte ()) (native (simple_tests$ext_lib))))
    (jsoo_runtime ())
    (requires ())
    (ppx_runtime_deps ())
    (pps ())
    (main_module_name Simple_tests)
    (sub_systems
     ((inline_tests.backend
       1.0
       ((flags :standard)
        (generate_runner
         (run sed "s/(\\*TEST:\\(.*\\)\\*)/let () = \\1;;/" %{impl-files}))))))))

  $ env OCAMLPATH=backend/_build/install/default/lib dune runtest --root example
  Entering directory 'example'
