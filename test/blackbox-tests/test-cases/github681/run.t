  $ jbuilder build foo
  Error: exception Sys_error("Is a directory")
  Backtrace:
  Raised at file "src/dep_path.ml" (inlined), line 46, characters 24-55
  Called from file "src/build_system.ml", line 91, characters 6-48
  Called from file "src/fiber/fiber.ml", line 303, characters 6-18
  [1]
