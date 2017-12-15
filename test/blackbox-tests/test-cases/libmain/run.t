  $ $JBUILDER runtest -j1 --root .
      ocamldep foo_rewriter.depends.ocamldep-output
        ocamlc foo_rewriter.{cmi,cmo,cmt}
      ocamlopt foo_rewriter.{cmx,o}
      ocamlopt foo_rewriter.{a,cmxa}
      ocamlopt .ppx/foo_rewriter/ppx.exe
           ppx foo_test_runner.pp.ml
      ocamldep foo_test_runner.depends.ocamldep-output
           ppx foo.pp.ml
      ocamldep foo_runner.depends.ocamldep-output
      ocamldep libmain/libmain.dependsi.ocamldep-output
      ocamldep libmain/libmain.depends.ocamldep-output
      ocamldep foo.depends.ocamldep-output
        ocamlc libmain/libmain.{cmi,cmti}
        ocamlc foo.{cmi,cmo,cmt}
        ocamlc foo_runner.{cmi,cmo,cmt}
      ocamlopt libmain/libmain.{cmx,o}
      ocamlopt foo.{cmx,o}
        ocamlc foo_test_runner.{cmi,cmo,cmt}
      ocamlopt foo_runner.{cmx,o}
      ocamlopt libmain/libmain.{a,cmxa}
      ocamlopt foo.{a,cmxa}
      ocamlopt foo_test_runner.{cmx,o}
      ocamlopt foo_runner.{a,cmxa}
      ocamlopt foo_test_runner.exe
  foo_test_runner alias runtest
  FOO
