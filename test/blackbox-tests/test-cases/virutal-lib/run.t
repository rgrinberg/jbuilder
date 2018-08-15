virtual library defined without an implementation

  $ dune build --root simple-virtual-lib
  Entering directory 'simple-virtual-lib'

virtual libraries may not implement their virtual modules

  $ dune build --root invalid-virtual-lib
  Entering directory 'invalid-virtual-lib'
  File "dune", line 3, characters 18-21:
   (virtual_modules foo bar))
                    ^^^
  Error: Module Foo has an implementation, it cannot be listed here
  [1]

virtual libraries may have multiple implementations

  $ dune build --root implementations
  Entering directory 'implementations'

implementations may be private

  $ dune build --root private-implementations
  Entering directory 'private-implementations'
      ocamlopt using_impl1.exe (exit 2)
  (cd _build/default && /Users/rgrinberg/.opam/4.06.1/bin/ocamlopt.opt -w @a-4-29-40-41-42-44-45-48-58-59-60-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -o using_impl1.exe -I impl1 -I vlib impl1/impl1.cmxa .using_impl1.eobjs/using_impl1.cmx)
  File "_none_", line 1:
  Error: No implementations provided for the following modules:
           Foo referenced from .using_impl1.eobjs/using_impl1.cmx
      ocamlopt using_impl2.exe (exit 2)
  (cd _build/default && /Users/rgrinberg/.opam/4.06.1/bin/ocamlopt.opt -w @a-4-29-40-41-42-44-45-48-58-59-60-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -o using_impl2.exe -I impl2 -I vlib impl2/impl2.cmxa .using_impl2.eobjs/using_impl2.cmx)
  File "_none_", line 1:
  Error: No implementations provided for the following modules:
           Foo referenced from .using_impl2.eobjs/using_impl2.cmx
  [1]

we cannot use more than implementation per lib in an executable

  $ dune exec ./foo.exe --root duplicate-implementations
  Info: creating file dune-project with this contents: (lang dune 1.1)
  Entering directory 'duplicate-implementations'
  Entering directory 'duplicate-implementations'
      ocamlopt foo.exe (exit 2)
  (cd _build/default && /Users/rgrinberg/.opam/4.06.1/bin/ocamlopt.opt -w @a-4-29-40-41-42-44-45-48-58-59-60-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -o foo.exe -I impl1 -I impl2 -I vlib impl1/impl1.cmxa impl2/impl2.cmxa .foo.eobjs/foo.cmx)
  File "_none_", line 1:
  Error: No implementations provided for the following modules:
           Vlib referenced from .foo.eobjs/foo.cmx
  [1]
