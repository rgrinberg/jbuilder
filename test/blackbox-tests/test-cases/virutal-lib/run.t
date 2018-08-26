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
  Multiple rules generated for _build/default/impl1/.impl1.objs/foo__Vlib.cmi:
  - <internal location>
  - <internal location>
  Multiple rules generated for _build/default/impl2/.impl2.objs/foo__Vlib.cmi:
  - <internal location>
  - <internal location>
  [1]

implementations may be private

  $ dune build --root private-implementations
  Entering directory 'private-implementations'
  Multiple rules generated for _build/default/impl1/.impl1.objs/foo.cmi:
  - <internal location>
  - <internal location>
  Multiple rules generated for _build/default/impl2/.impl2.objs/foo.cmi:
  - <internal location>
  - <internal location>
        ocamlc .using_impl1.eobjs/using_impl1.{cmi,cmo,cmt} (exit 2)
  (cd _build/default && /Users/rgrinberg/.opam/4.06.1/bin/ocamlc.opt -w @a-4-29-40-41-42-44-45-48-58-59-60-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -bin-annot -I .using_impl1.eobjs -I impl1/.impl1.objs -no-alias-deps -opaque -o .using_impl1.eobjs/using_impl1.cmo -c -impl using_impl1.ml)
  File "using_impl1.ml", line 1, characters 9-20:
  Error: Unbound module Foo
        ocamlc .using_impl2.eobjs/using_impl2.{cmi,cmo,cmt} (exit 2)
  (cd _build/default && /Users/rgrinberg/.opam/4.06.1/bin/ocamlc.opt -w @a-4-29-40-41-42-44-45-48-58-59-60-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -bin-annot -I .using_impl2.eobjs -I impl2/.impl2.objs -no-alias-deps -opaque -o .using_impl2.eobjs/using_impl2.cmo -c -impl using_impl2.ml)
  File "using_impl2.ml", line 1, characters 9-20:
  Error: Unbound module Foo
  [1]

we cannot use more than implementation per lib in an executable

  $ dune exec ./foo.exe --root duplicate-implementations
  Info: creating file dune-project with this contents: (lang dune 1.1)
  Internal error, please report upstream including the contents of _build/log.
  Description:
  ("Syntax identifier is unset"
   (name variants)
   (supported_versions ((1 0)))
   (context
    ((dune 1.1)
     (dune-project
      ((name (anonymous (In_source_tree .)))
       (root .)
       (version ())
       (project_file ((file (In_source_tree dune-project)) (exists true)))
       (kind dune))))))
  Backtrace:
  Raised at file "src/stanza.ml", line 75, characters 23-32
  Called from file "src/dsexp/dsexp.ml", line 370, characters 19-30
  Called from file "src/dsexp/dsexp.ml", line 680, characters 17-31
  Called from file "src/dsexp/dsexp.ml" (inlined), line 488, characters 24-53
  Called from file "src/dsexp/dsexp.ml", line 689, characters 4-609
  Called from file "src/dsexp/dsexp.ml", line 462, characters 15-29
  Called from file "list.ml", line 82, characters 20-23
  Called from file "src/stdune/list.ml", line 37, characters 29-39
  Called from file "src/dune_file.ml", line 1897, characters 4-79
  Called from file "src/dune_file.ml", line 1923, characters 8-77
  Called from file "src/jbuild_load.ml", line 14, characters 18-57
  Called from file "src/jbuild_load.ml", line 231, characters 8-113
  Called from file "src/jbuild_load.ml", line 276, characters 12-74
  Called from file "map.ml", line 315, characters 19-42
  Called from file "src/jbuild_load.ml", line 284, characters 16-46
  Called from file "src/main.ml", line 44, characters 4-70
  Called from file "src/main.ml", line 278, characters 12-56
  Called from file "bin/main.ml", line 114, characters 4-327
  Called from file "bin/main.ml", line 1223, characters 42-66
  Called from file "vendor/cmdliner/src/cmdliner_term.ml", line 27, characters 19-24
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 106, characters 32-39
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 136, characters 18-36
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 251, characters 22-48
  Called from file "bin/main.ml", line 1674, characters 10-51
  
  I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
  the little-death that brings total obliteration.  I will fully express
  my cases.  Execution will pass over me and through me.  And when it
  has gone past, I will unwind the stack along its path.  Where the
  cases are handled there will be nothing.  Only I will remain.
  [1]
