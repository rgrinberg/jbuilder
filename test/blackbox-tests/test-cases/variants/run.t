Test that trying to specify a variant for not an implementation results in an 
appropriate error message.
  $ dune build --root variant-not-implementation
  Entering directory 'variant-not-implementation'
  File "dune", line 4, characters 13-16:
  4 |     (variant foo)
                   ^^^
  Error: Only implementations can specify a variant.
  [1]

Having multiple implementations of the same library with respect to selected 
variants results in an appropriate error message.
  $ dune build --root multiple-implementations-for-variants
  Entering directory 'multiple-implementations-for-variants'
  Error: Multiple solutions for the implementation of vlib 
  with variants [ "default" ]
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


Check that ambiguity is handled correctly.
  $ dune build --root dependency-cycle
  Entering directory 'dependency-cycle'
  Error: Default implementation cycle detected between the following libraries:
  -> "clock"
  -> "clock_ocaml"
  -> "async_ocaml"
  -> "async"
  -> "async_c"
  -> "clock_c"
  -> "clock"
  -> "test_default"
  -> "test"
  -> required by executable bar in dune:2
  [1]

Solving variant ambiguity by specifying a concrete implementation.
  $ dune build --root variant-with-concrete-impl
  Entering directory 'variant-with-concrete-impl'
           bar alias default
  hello from lib2.default
