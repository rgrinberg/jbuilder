  $ dune build --display short @install
      ocamldep alib/alib.ml.d
      ocamldep blib/sub/sub.ml.d
        ocamlc blib/sub/.sub.objs/sub.{cmi,cmo,cmt}
      ocamlopt blib/sub/.sub.objs/sub.{cmx,o}
      ocamlopt blib/sub/sub.{a,cmxa}
      ocamlopt blib/sub/sub.cmxs
      ocamldep blib/blib.ml.d
        ocamlc blib/.blib.objs/blib.{cmi,cmo,cmt}
      ocamlopt blib/.blib.objs/blib.{cmx,o}
      ocamlopt blib/blib.{a,cmxa}
      ocamlopt blib/blib.cmxs
        ocamlc alib/.alib.objs/alib__.{cmi,cmo,cmt}
      ocamlopt alib/.alib.objs/alib__.{cmx,o}
      ocamldep alib/main.ml.d
        ocamlc alib/.alib.objs/alib__Main.{cmi,cmo,cmt}
      ocamlopt alib/.alib.objs/alib__Main.{cmx,o}
        ocamlc blib/sub/sub.cma
        ocamlc blib/blib.cma
        ocamlc alib/.alib.objs/alib.{cmi,cmo,cmt}
      ocamlopt alib/.alib.objs/alib.{cmx,o}
      ocamlopt alib/alib.{a,cmxa}
      ocamlopt alib/alib.cmxs
        ocamlc alib/alib.cma
