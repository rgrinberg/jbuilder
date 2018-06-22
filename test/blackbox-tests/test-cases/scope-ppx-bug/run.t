  $ dune build --display short @install --debug-dep
  Internal error, please report upstream including the contents of _build/log.
  Description:
  ("Syntax identifier is unset" (name dune))
  Backtrace:
  Raised at file "src/stdune/exn.ml", line 32, characters 5-10
  Called from file "src/stdune/sexp.ml", line 128, characters 5-8
  Called from file "src/stdune/sexp.ml" (inlined), line 124, characters 19-30
  Called from file "src/string_with_vars.ml", line 87, characters 2-563
  Called from file "src/stdune/sexp.ml", line 206, characters 15-29
  Called from file "src/ordered_set_lang.ml", line 259, characters 20-69
  Called from file "list.ml", line 82, characters 20-23
  Called from file "src/ordered_set_lang.ml", line 278, characters 25-47
  Called from file "src/ordered_set_lang.ml", line 282, characters 19-31
  Called from file "src/super_context.ml", line 116, characters 6-80
  Called from file "src/preprocessing.ml", line 561, characters 11-166
  Called from file "src/stdune/result.ml", line 26, characters 15-20
  Called from file "src/stdune/result.ml" (inlined), line 39, characters 20-29
  Called from file "src/preprocessing.ml", line 555, characters 8-416
  Called from file "array.ml", line 101, characters 21-40
  Called from file "src/per_item.ml", line 57, characters 22-43
  Called from file "src/gen_rules.ml", line 544, characters 6-301
  Called from file "src/stdune/exn.ml", line 21, characters 8-11
  Re-raised at file "src/stdune/exn.ml", line 23, characters 30-37
  Called from file "src/gen_rules.ml", line 973, characters 15-107
  Called from file "src/stdune/list.ml", line 13, characters 10-13
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
