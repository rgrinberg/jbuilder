  $ $JBUILDER external-lib-deps --root . -j1 --display quiet @install
  These are the external library dependencies in the default context:
  - a
  - b
  - c

Reproduction case for #484. The error should point to src/jbuild

  $ $JBUILDER build --root . -j1 --display quiet @install
  File "src/jbuild", line 1, characters 0-66:
  Error: Library "a" not found.
  Hint: try: jbuilder external-lib-deps --missing --root . @install
  [1]
