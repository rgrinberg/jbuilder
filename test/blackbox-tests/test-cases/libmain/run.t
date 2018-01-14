  $ $JBUILDER runtest -j1 --root .
      ocamldep foo_rewriter.depends.ocamldep-output
      ocamldep foo_runner.depends.ocamldep-output
      ocamldep libmain/libmain.depends.ocamldep-output
      ocamldep libmain/libmain_main.depends.ocamldep-output
        ocamlc foo_rewriter.{cmi,cmo,cmt}
      ocamldep libmain/libmain.dependsi.ocamldep-output
      ocamlopt foo_rewriter.{cmx,o}
        ocamlc libmain/libmain.{cmi,cmti}
      ocamlopt foo_rewriter.{a,cmxa}
        ocamlc libmain/libmain_main.{cmi,cmo,cmt}
      ocamlopt libmain/libmain.{cmx,o}
      ocamlopt .ppx/foo_rewriter/ppx.exe
      ocamlopt libmain/libmain_main.{cmx,o}
      ocamlopt libmain/libmain.{a,cmxa}
           ppx foo.pp.ml
      ocamlopt libmain/libmain_main.{a,cmxa}
      ocamldep foo.depends.ocamldep-output
        ocamlc foo.{cmi,cmo,cmt}
        ocamlc foo_runner.{cmi,cmo,cmt}
      ocamlopt foo.{cmx,o}
      ocamlopt foo_runner.{cmx,o}
      ocamlopt foo.{a,cmxa}
      ocamlopt foo_runner.{a,cmxa}
      ocamlopt foo_test_runner.exe
  foo_test_runner alias runtest
  FOO
