  $ dune runtest --display short
      ocamldep bar.ml.d
      ocamldep foo.ml.d
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
        ocamlc .bar.objs/bar.{cmi,cmo,cmt}
        ocamlc bar.cma
      ocamlopt .foo.objs/foo.{cmx,o}
      ocamlopt .bar.objs/bar.{cmx,o}
      ocamlopt bar.{a,cmxa}
      ocamlopt bar.cmxs
        ocamlc foo.cma
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs
     ocamlfind test.exe
          test alias runtest
  42 42
