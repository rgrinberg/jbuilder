  $ dune build @print-merlins --display short
      ocamldep sanitize-dot-merlin/sanitize_dot_merlin.ml.d
        ocamlc sanitize-dot-merlin/.sanitize_dot_merlin.eobjs/sanitize_dot_merlin.{cmo,cmt,cmi}
      ocamlopt sanitize-dot-merlin/.sanitize_dot_merlin.eobjs/sanitize_dot_merlin.{cmx,o}
      ocamlopt sanitize-dot-merlin/sanitize_dot_merlin.exe
  sanitize_dot_merlin alias print-merlins
  # Processing exe/.merlin
  B $LIB_PREFIX/lib/bytes
  B $LIB_PREFIX/lib/findlib
  B $LIB_PREFIX/lib/ocaml
  B ../_build/default/exe/.x.eobjs
  B ../lib/.foo.objs
  S $LIB_PREFIX/lib/bytes
  S $LIB_PREFIX/lib/findlib
  S $LIB_PREFIX/lib/ocaml
  S ../_build/default/lib
  S .
  FLG -w -40
  # Processing lib/.merlin
  B $LIB_PREFIX/lib/bytes
  B $LIB_PREFIX/lib/findlib
  B $LIB_PREFIX/lib/ocaml
  B ../_build/default/lib/.bar.objs
  B ../_build/default/lib/.foo.objs
  S $LIB_PREFIX/lib/bytes
  S $LIB_PREFIX/lib/findlib
  S $LIB_PREFIX/lib/ocaml
  S .
  FLG -ppx '$PPX/fooppx@./ppx.exe --as-ppx --cookie '\''library-name="foo"'\'''
  FLG -open Foo -w -40 -open Bar -w -40

Make sure a ppx directive is generated

  $ grep -q ppx lib/.merlin
