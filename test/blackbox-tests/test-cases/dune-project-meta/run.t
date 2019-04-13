Test the various new fields inside the dune-project file.

  $ cp proj dune-project && touch cohttp.opam && touch cohttp-async.opam

The `dune build` should work.

  $ dune build @install --auto-promote
  File "cohttp-async.opam", line 1, characters 0-0:
  Files _build/default/cohttp-async.opam and _build/default/cohttp-async.opam.expected differ.
  File "cohttp.opam", line 1, characters 0-0:
  Files _build/default/cohttp.opam and _build/default/cohttp.opam.expected differ.
  Promoting _build/default/cohttp-async.opam.expected to cohttp-async.opam.
  Promoting _build/default/cohttp.opam.expected to cohttp.opam.
  [1]
  $ cat cohttp.opam
  dev-repo: "git+https://github.com/mirage/ocaml-cohttp.git"
  authors: ["Anil Madhavapeddy" "Rudi Grinberg"]
  license: "ISC"
  description: "A longer description"
  synopsis: "An OCaml library for HTTP clients and servers"
  $ cat cohttp-async.opam
  dev-repo: "git+https://github.com/mirage/ocaml-cohttp.git"
  authors: ["Anil Madhavapeddy" "Rudi Grinberg"]
  license: "ISC"
  description: "A _really_ long description"
  synopsis: "HTTP client and server for the Async library"


