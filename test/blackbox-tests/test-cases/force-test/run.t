  $ $JBUILDER clean -j0 --root .
  $ $JBUILDER runtest -j0 --root .
      ocamldep f.depends.ocamldep-output
        ocamlc f.{cmi,cmo,cmt}
      ocamlopt f.{cmx,o}
      ocamlopt f.exe
             f alias runtest
  Foo Bar
  $ $JBUILDER runtest -j0 --root .
  $ $JBUILDER runtest --force -j0 --root .
             f alias runtest
  Foo Bar
