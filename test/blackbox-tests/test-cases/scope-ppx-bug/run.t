  $ dune build --display short @install --debug-dep
  Internal error, please report upstream including the contents of _build/log.
  Description:
  ("Syntax identifier is unset" (name dune))
  Backtrace:
  Raised at file "src/stdune/exn.ml", line 32, characters 5-10
  Called from file "src/stdune/sexp.ml", line 128, characters 5-8
  Called from file "src/stdune/sexp.ml" (inlined), line 124, characters 19-30
  Called from file "src/ordered_set_lang.ml", line 180, characters 4-811
  Called from file "src/stdune/sexp.ml", line 460, characters 25-39
  Called from file "src/stdune/sexp.ml" (inlined), line 124, characters 19-30
  Called from file "src/preprocessing.ml", line 48, characters 11-444
  Called from file "src/stdune/sexp.ml", line 514, characters 23-55
  Called from file "src/stdune/sexp.ml", line 251, characters 19-28
  Called from file "src/stdune/sexp.ml" (inlined), line 230, characters 24-53
  Called from file "src/stdune/sexp.ml", line 247, characters 4-232
  Called from file "src/stdune/sexp.ml", line 204, characters 15-29
  Called from file "src/preprocessing.ml", line 188, characters 6-122
  Called from file "camlinternalLazy.ml", line 27, characters 17-27
  Re-raised at file "camlinternalLazy.ml", line 34, characters 4-11
  Called from file "src/preprocessing.ml", line 224, characters 8-27
  Called from file "src/preprocessing.ml", line 391, characters 17-45
  Called from file "src/preprocessing.ml", line 555, characters 8-53
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
