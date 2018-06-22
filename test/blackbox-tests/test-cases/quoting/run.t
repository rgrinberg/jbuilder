This behavior is surprising, we should get an error about the fact
that ${@} is not quoted and doesn't contain exactly 1 element

  $ dune build --root bad x
  Entering directory 'bad'
  File "dune", line 3, characters 28-30:
  Error: Variable %{@} expands to 2 values, however a single value is expected here. Please quote this atom.
  [1]

The targets should only be interpreted as a single path when quoted

  $ dune build --root good s
  Entering directory 'good'
  Error: Rule failed to generate the following targets:
  - s
  - t
  [1]

  $ dune runtest --root quote-from-context
  Entering directory 'quote-from-context'
    count_args alias runtest
  Number of args: 3

  $ dune runtest --root quotes-multi
  Entering directory 'quotes-multi'
  lines: foo bar baz

  $ dune build @quoted --root filename-space
  File "dune", line 4, characters 18-19:
  Error: unexpected variable
  [1]

  $ dune build @unquoted --root filename-space
  File "dune", line 4, characters 18-19:
  Error: unexpected variable
  [1]
