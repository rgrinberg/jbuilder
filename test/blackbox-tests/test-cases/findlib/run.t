  $ $JBUILDER external-lib-deps --root . -j1 --display quiet @install
  These are the external library dependencies in the default context:
  - a
  - b
  - c

Reproduction case for #484. The error should point to src/jbuild

  $ $JBUILDER build --root . -j1 --display quiet @install
  Error: Library "a" not found.
  -> required by library "foo" in _build/default/src
  -> required by META.foo
  Hint: try: jbuilder external-lib-deps --missing --root . @install
  File "src/jbuild", line 2, characters 1-64:
  Error: Library "a" not found.
  -> required by library "foo" in _build/default/src
  Hint: try: jbuilder external-lib-deps --missing --root . @install
  [1]
