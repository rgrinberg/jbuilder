  $ dune build 2>&1 | grep -v ocamlc
  File "fooexe.ml", line 3, characters 0-7:
  Error (warning 3): deprecated: module Bar
  Will be removed past 2020-20-20. Use Mylib.Bar instead.
