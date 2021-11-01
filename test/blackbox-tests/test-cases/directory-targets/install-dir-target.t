Allow directories to be installable

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (package (name foo))
  > (using directory-targets 0.1)
  > EOF

  $ cat >dune <<EOF
  > (install
  >  (dirs rules/bar)
  >  (section share))
  > EOF

  $ mkdir rules
  $ cat >rules/dune <<EOF
  > (rule
  >  (target (dir bar))
  >  (action (bash "mkdir -p %{target}/baz && touch %{target}/{x,y,z} && touch %{target}/baz/{a,b}")))
  > EOF

  $ dune build foo.install
  Error: The package foo does not have any user defined stanzas attached to it.
  If this is intentional, add (allow_empty) to the package definition in the
  dune-project file
  -> required by _build/default/foo.install
  [1]
  $ cat _build/default/foo.install
  cat: _build/default/foo.install: No such file or directory
  [1]
