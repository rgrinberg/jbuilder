Test that trying to specify a variant for not an implementation results in an
appropriate error message.
  $ dune build --root variant-not-implementation
  Entering directory 'variant-not-implementation'
  File "dune", line 4, characters 10-13:
  4 |  (variant foo))
                ^^^
  Error: Only implementations can specify a variant.
  [1]

Having multiple implementations of the same library with respect to selected
variants results in an appropriate error message.
  $ dune build --root multiple-implementations-for-variants
  Entering directory 'multiple-implementations-for-variants'
  File "lib/dune", line 1, characters 0-63:
  1 | (library
  2 |  (name vlib)
  3 |  (virtual_modules vlib)
  4 |  (wrapped false))
  Error: Multiple solutions for the implementation
  of vlib  with variants [ "default" ]
  -> lib2_default ("default")
  -> lib_default ("default")
  -> required by executable bar in dune:2
  [1]

Basic sample using variants and a default library.
  $ dune build --root variants-base
  Entering directory 'variants-base'
           bar alias default
  hello from lib.test

Check that implementations are chosen according to manual specification, then
variants and finally default implementation.
  $ dune build --root resolution-priority
  Entering directory 'resolution-priority'
           bar alias default
  hi from direct.ocaml
  hi from variant.c
  hi from test.default

