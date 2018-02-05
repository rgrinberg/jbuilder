  $ $JBUILDER clean -j0 --root .
  $ $JBUILDER build -j0 --root . @just-in-src
  running in src
  $ $JBUILDER clean -j0 --root .
  $ $JBUILDER build -j0 --root . @everywhere
  running in src/foo/bar
  running in src/foo/baz
  running in src
  $ $JBUILDER clean -j0 --root .
  $ $JBUILDER build -j0 --root . @x
  running in src/foo/bar
  running in src/foo/baz
  running in src
  $ $JBUILDER build -j0 --root . @plop
  From the command line:
  Error: Alias plop is empty.
  It is not defined in . or any of its descendants.
  [1]
  $ $JBUILDER build -j0 --root . @truc/x
  From the command line:
  Error: Don't know about directory truc!
  [1]
