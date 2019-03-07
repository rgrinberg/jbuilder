Test that trying to specify a default implementation for a non-virtual library results
in an appropriate error message.
  $ dune build --root default-impl-not-virtual-lib
  Entering directory 'default-impl-not-virtual-lib'
  File "dune", line 4, characters 28-36:
  4 |     (default_implementation lib.impl))
                                  ^^^^^^^^
  Error: Only virtual libraries can specify a default implementation.
  [1]

Basic sample selecting implementation according to default library.
  $ dune build --root default-impl
  Entering directory 'default-impl'
           bar alias default
  hi from lib.default
