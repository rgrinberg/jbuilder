Reproduce bug in #3636.

The bug occurs when (include_subdirs unqualified) is used.

  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > (package
  >  (name testpkg))
  > EOF

  $ mkdir vlib impl

  $ cat >vlib/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name vlib)
  >  (public_name testpkg.vlib)
  >  (virtual_modules greet))
  > EOF
  $ cat >vlib/greet.mli <<EOF
  > val s : string
  > EOF

  $ cat >impl/dune <<EOF
  > (library
  >  (name impl)
  >  (public_name testpkg.impl)
  >  (implements vlib))
  > EOF
  $ cat >impl/greet.ml <<EOF
  > let s = "hello world"
  > EOF

  $ dune build @install

  $ mkdir exe
  $ cat >exe/dune-project <<EOF
  > (lang dune 2.7)
  > EOF

  $ cat >exe/dune <<EOF
  > (executable
  >  (name foo)
  >  (libraries testpkg.vlib testpkg.impl))
  > EOF

  $ cat >exe/foo.ml <<EOF
  > print_endline Vlib.Greet.s
  > EOF
  $ OCAMLPATH=$PWD/_build/install/default/lib dune exec --root exe -- ./foo.exe
  Entering directory 'exe'
  Entering directory 'exe'
  File "foo.ml", line 1, characters 14-26:
  1 | print_endline Vlib.Greet.s
                    ^^^^^^^^^^^^
  Error: Unbound module Vlib
  [1]
