  $ echo '(jbuild_version 1)' > dune
  $ dune build
  Info: creating file dune-project with this contents: (lang dune 1.0)
  File "dune", line 1, characters 0-18:
  Error: 'jbuild_version' was deleted in version 1.0 of the dune language
  [1]
  $ rm -f dune

  $ echo '(jbuild_version 1)' > jbuild
  $ dune build
  $ rm -f jbuild

  $ echo '(executable ((name x) (link_executables false)))' > dune
  $ dune build
  File "dune", line 1, characters 22-46:
  Error: 'link_executables' was deleted in version 1.0 of the dune language
  [1]
  $ rm -f dune
