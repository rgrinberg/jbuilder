  $ $JBUILDER build -j1 --root . @install
      ocamldep alib/alib.depends.ocamldep-output
        ocamlc alib/alib__.{cmi,cmo,cmt}
      ocamldep blib/blib.depends.ocamldep-output
      ocamldep blib/sub/sub.depends.ocamldep-output
      ocamlopt alib/alib__.{cmx,o}
        ocamlc blib/sub/sub.{cmi,cmo,cmt}
      ocamlopt blib/sub/sub.{cmx,o}
        ocamlc blib/blib.{cmi,cmo,cmt}
        ocamlc blib/sub/sub.cma
      ocamlopt blib/sub/sub.{a,cmxa}
      ocamlopt blib/blib.{cmx,o}
        ocamlc alib/alib__Main.{cmi,cmo,cmt}
        ocamlc alib/alib.{cmi,cmo,cmt}
        ocamlc blib/blib.cma
      ocamlopt blib/sub/sub.cmxs
      ocamlopt blib/blib.{a,cmxa}
      ocamlopt alib/alib__Main.{cmx,o}
      ocamlopt alib/alib.{cmx,o}
        ocamlc alib/alib.cma
      ocamlopt blib/blib.cmxs
      ocamlopt alib/alib.{a,cmxa}
      ocamlopt alib/alib.cmxs
