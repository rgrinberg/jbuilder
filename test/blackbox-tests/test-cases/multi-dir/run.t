Simple test with a multi dir exe
--------------------------------

  $ dune build --root test1
  Entering directory 'test1'
           foo alias default
  Hello, world!

Test that executables stop the recursion
----------------------------------------

  $ dune build --root test2
  Entering directory 'test2'
          main alias default
  Hello, world!

Test some error cases
---------------------

  $ dune build --root error1
  Entering directory 'error1'
  File "dune", line 1, characters 0-0:
  Error: Module "X" appears in several directories:
  - _build/default/b
  - _build/default/a
  [1]

  $ dune build --root error2
  Entering directory 'error2'
  File "dune", line 2, characters 0-23:
  Error: The 'include_subdirs' stanza cannot appear more than once
  [1]
