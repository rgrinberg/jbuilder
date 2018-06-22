  $ dune runtest --display short
  Internal error, please report upstream including the contents of _build/log.
  Description:
  ("Syntax identifier is unset" (name dune))
  Backtrace:
  Raised at file "src/dep_path.ml" (inlined), line 44, characters 24-55
  Called from file "src/build_system.ml", line 89, characters 6-48
  Called from file "src/fiber/fiber.ml", line 240, characters 6-18
      ocamldep bar.ml.d
      ocamldep foo.ml.d
      ocamldep foo.mli.d
        ocamlc .foo.objs/foo.{cmi,cmti}
      ocamlopt .foo.objs/foo.{cmx,o}
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs
      ocamldep foo_byte.ml.d
        ocamlc .foo_byte.objs/foo_byte.{cmi,cmo,cmt}
        ocamlc foo_byte.cma
      ocamldep ppx-new/foo_ppx_rewriter_dune.ml.d
        ocamlc ppx-new/.foo_ppx_rewriter_dune.objs/foo_ppx_rewriter_dune.{cmi,cmo,cmt}
      ocamlopt ppx-new/.foo_ppx_rewriter_dune.objs/foo_ppx_rewriter_dune.{cmx,o}
      ocamlopt ppx-new/foo_ppx_rewriter_dune.{a,cmxa}
      ocamlopt ppx-new/foo_ppx_rewriter_dune.cmxs
      ocamldep ppx-old/foo_ppx_rewriter_jbuild.ml.d
        ocamlc ppx-old/.foo_ppx_rewriter_jbuild.objs/foo_ppx_rewriter_jbuild.{cmi,cmo,cmt}
      ocamlopt ppx-old/.foo_ppx_rewriter_jbuild.objs/foo_ppx_rewriter_jbuild.{cmx,o}
      ocamlopt ppx-old/foo_ppx_rewriter_jbuild.{a,cmxa}
      ocamlopt ppx-old/foo_ppx_rewriter_jbuild.cmxs
        ocamlc .foo.objs/foo.{cmo,cmt}
        ocamlc foo.cma
        ocamlc .bar.eobjs/bar.{cmi,cmo,cmt}
      ocamlopt .bar.eobjs/bar.{cmx,o}
      ocamlopt bar.exe
        ocamlc ppx-new/foo_ppx_rewriter_dune.cma
      ocamlopt .ppx/foo.ppx_rewriter_dune/ppx.exe
        ocamlc ppx-old/foo_ppx_rewriter_jbuild.cma
  [1]
