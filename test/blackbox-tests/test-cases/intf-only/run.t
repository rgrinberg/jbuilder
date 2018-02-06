Successes:

  $ $JBUILDER build --root foo -j1 --debug-dep 2>&1 | grep -v Entering
      ocamldep test/bar.ml.d
      ocamldep foo.ml.d
        ocamlc foo__.{cmi,cmti}
      ocamldep intf.mli.d
        ocamlc foo__Intf.{cmi,cmti}
        ocamlc foo.{cmi,cmo,cmt}
      ocamlopt foo.{cmx,o}
        ocamlc foo.cma
        ocamlc test/bar.{cmi,cmo,cmt}
      ocamlopt foo.{a,cmxa}
      ocamlopt test/bar.{cmx,o}
  [1mFile "[1m_none_", line 1[0m[0m:
  [1;35mWarning[0m 58: no cmx file was found in path for module Foo__Intf, and its interface was not compiled with -opaque
        ocamlc test/bar.cma
      ocamlopt foo.cmxs
      ocamlopt test/bar.{a,cmxa}
      ocamlopt test/bar.cmxs

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
