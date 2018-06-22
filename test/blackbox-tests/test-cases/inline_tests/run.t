  $ env -u OCAMLRUNPARAM jbuilder runtest simple
           run alias simple/runtest (exit 2)
  (cd _build/default/simple && .foo_simple.inline-tests/run.exe)
  Fatal error: exception File "simple/.foo_simple.inline-tests/run.ml", line 1, characters 10-16: Assertion failed
  [1]

  $ dune runtest missing-backend
  File "missing-backend/dune", line 3, characters 2-16:
  Error: No inline tests backend found.
  [1]

  $ dune runtest too-many-backends
  File "too-many-backends/dune", line 17, characters 2-16:
  Error: Too many independant inline tests backends found:
  - "backend_tmb1" in _build/default/too-many-backends
  - "backend_tmb2" in _build/default/too-many-backends
  [1]

  $ dune runtest many-backends-choose
           run alias many-backends-choose/runtest
  backend_mbc1

  $ dune runtest dune-file
  File "src/string_with_vars.ml", line 90, characters 20-26: Assertion failed
  Backtrace:
  Raised at file "src/string_with_vars.ml", line 90, characters 20-32
  Called from file "src/stdune/sexp.ml", line 128, characters 5-8
  Called from file "src/stdune/sexp.ml", line 204, characters 15-29
  Called from file "src/ordered_set_lang.ml", line 254, characters 35-75
  Called from file "list.ml", line 82, characters 20-23
  Called from file "list.ml", line 82, characters 32-39
  Called from file "src/ordered_set_lang.ml", line 272, characters 25-47
  Called from file "src/ordered_set_lang.ml", line 276, characters 19-31
  Called from file "src/super_context.ml", line 116, characters 6-80
  Called from file "list.ml", line 82, characters 20-23
  Called from file "src/inline_tests.ml", line 249, characters 18-228
  Called from file "list.ml", line 100, characters 12-15
  Called from file "src/gen_rules.ml", line 767, characters 4-152
  Called from file "src/stdune/exn.ml", line 21, characters 8-11
  Re-raised at file "src/stdune/exn.ml", line 23, characters 30-37
  Called from file "src/gen_rules.ml", line 973, characters 15-107
  Called from file "src/stdune/list.ml", line 13, characters 10-13
  Called from file "src/stdune/list.ml", line 15, characters 21-36
  Called from file "src/gen_rules.ml", line 969, characters 6-867
  Called from file "src/gen_rules.ml", line 1025, characters 19-30
  Called from file "src/build_system.ml", line 884, characters 6-62
  Called from file "src/build_system.ml", line 860, characters 6-59
  Re-raised at file "src/build_system.ml", line 871, characters 6-17
  Called from file "src/build_system.ml" (inlined), line 828, characters 32-63
  Called from file "src/build_system.ml", line 838, characters 4-24
  Called from file "src/build_interpret.ml", line 99, characters 24-40
  Called from file "src/build_interpret.ml", line 58, characters 31-43
  Called from file "src/build_interpret.ml", line 58, characters 31-43
  Called from file "src/build_system.ml", line 1198, characters 10-108
  Called from file "src/fiber/fiber.ml", line 296, characters 6-13
  [1]
