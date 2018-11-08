  $ jbuilder build --root pre-1.6 data/dune
  Entering directory 'pre-1.6'
  $ jbuilder build --root pre-1.6 old-style/data/dune
  Entering directory 'pre-1.6'
  $ dune build --root 1.6 @runtest
  Entering directory '1.6'
  real dir
  $ dune build --root glob @runtest
  Entering directory 'glob'
  real dir
  $ dune build --root per-workspace --workspace per-workspace/dune-workspace
  File "garbage/dune", line 1, characters 0-7:
  1 | garbage
      ^^^^^^^
  Error: Unknown constructor garbage
  [1]
