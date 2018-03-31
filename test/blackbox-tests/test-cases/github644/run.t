  $ $JBUILDER runtest --root . -j 1 --display quiet
  File "jbuild", line 4, characters 20-42:
  Error: Library "ppx_that_doesn't_exist" not found.
  Hint: try: jbuilder external-lib-deps --missing --root . @runtest
  [1]

These should print something:

  $ $JBUILDER external-lib-deps --root . -j 1 --display quiet @runtest
  These are the external library dependencies in the default context:
  - ocaml-migrate-parsetree.driver-main
  - ppx_that_doesn't_exist

  $ $JBUILDER external-lib-deps --root . -j 1 --display quiet --missing @runtest
  Error: The following libraries are missing in the default context:
  - ppx_that_doesn't_exist
  Hint: try: opam install ppx_that_doesn't_exist
  [1]
