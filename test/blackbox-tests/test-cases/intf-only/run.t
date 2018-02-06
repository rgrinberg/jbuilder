Successes:

  $ $JBUILDER build --root foo -j1 --debug-dep 2>&1 | grep -v Entering
      ocamldep foo.ml.d
  No rule found for foo__Intf.cmx
  Dependency path:
      alias install
  --> foo.install
  --> default/lib/foo/foo.a (context install)
  --> foo.a

Errors:

  $ $JBUILDER build --root a -j1 foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 2, characters 1-13:
  Warning: Some modules don't have an implementation.
  You need to add the following field to this stanza:
  
    (modules_without_implementation (x y))
  
  This will become an error in the future.
        ocamlc foo.{cmi,cmo,cmt}
        ocamlc foo.cma
  $ $JBUILDER build --root b -j1 foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 3, characters 34-37:
  Warning: The following modules must be listed here as they don't have an implementation:
  - y
  This will become an error in the future.
        ocamlc foo.{cmi,cmo,cmt}
        ocamlc foo.cma
  $ $JBUILDER build --root c -j1 foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 1, characters 0-58:
  Error: Module X doesn't exist.
  $ $JBUILDER build --root d -j1 foo.cma 2>&1 | grep -v Entering
  File "jbuild", line 1, characters 0-58:
  Error: Module X has an implementation, it cannot be listed here
