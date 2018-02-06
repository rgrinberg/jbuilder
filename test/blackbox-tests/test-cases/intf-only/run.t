  $ $JBUILDER build --root a -j1 foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 2, characters 1-13:
  Warning: Some modules don't have an implementation.
  You need to add the following field to this stanza:
  
    (modules_without_implementation (x y))
  
  This will become an error in the future.
      ocamldep foo.depends.ocamldep-output
  Internal error, please report upstream including the contents of _build/log.
  Description: "X" not found in map <dependency graph in _build/default>
  Backtrace:
  Raised at file "src/import.ml", line 41, characters 43-65
  Called from file "src/top_closure.ml", line 24, characters 24-44
  Called from file "src/top_closure.ml", line 33, characters 14-42
  Called from file "src/top_closure.ml", line 37, characters 10-58
  Called from file "src/ocamldep.ml", line 94, characters 8-54
  Called from file "src/ocamldep.ml", line 102, characters 11-45
  Called from file "src/build_system.ml", line 414, characters 16-33
  Called from file "src/build_system.ml", line 401, characters 8-25
  Called from file "src/build_system.ml", line 401, characters 8-25
  Called from file "src/build_system.ml", line 401, characters 8-25
  Called from file "src/build_system.ml", line 401, characters 8-25
  Called from file "src/build_system.ml", line 448, characters 17-47
  Called from file "src/future.ml", line 88, characters 40-45
  Called from file "src/future.ml", line 83, characters 65-70
  Called from file "src/future.ml", line 37, characters 9-12
  $ $JBUILDER build --root b -j1 foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 3, characters 34-37:
  Warning: The following modules must be listed here as they don't have an implementation:
  - y
  This will become an error in the future.
      ocamldep foo.depends.ocamldep-output
  Internal error, please report upstream including the contents of _build/log.
  Description: "X" not found in map <dependency graph in _build/default>
  Backtrace:
  Raised at file "src/import.ml", line 41, characters 43-65
  Called from file "src/top_closure.ml", line 24, characters 24-44
  Called from file "src/top_closure.ml", line 33, characters 14-42
  Called from file "src/top_closure.ml", line 37, characters 10-58
  Called from file "src/ocamldep.ml", line 94, characters 8-54
  Called from file "src/ocamldep.ml", line 102, characters 11-45
  Called from file "src/build_system.ml", line 414, characters 16-33
  Called from file "src/build_system.ml", line 401, characters 8-25
  Called from file "src/build_system.ml", line 401, characters 8-25
  Called from file "src/build_system.ml", line 401, characters 8-25
  Called from file "src/build_system.ml", line 401, characters 8-25
  Called from file "src/build_system.ml", line 448, characters 17-47
  Called from file "src/future.ml", line 88, characters 40-45
  Called from file "src/future.ml", line 83, characters 65-70
  Called from file "src/future.ml", line 37, characters 9-12
  $ $JBUILDER build --root c -j1 foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 1, characters 0-58:
  Error: This module doesn't exist.
  $ $JBUILDER build --root d -j1 foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 3, characters 34-37:
  Error: The following modules must be listed here as they don't have an implementation:
  - x
