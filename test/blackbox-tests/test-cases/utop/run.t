  $ dune utop --display short forutop -- init_forutop.ml
      ocamldep forutop/.utop/utop.ml.d
      ocamldep forutop/forutop.ml.d
        ocamlc forutop/.forutop.objs/forutop.{cmo,cmt,cmi}
        ocamlc forutop/forutop.cma
        ocamlc forutop/.utop/utop.{cmo,cmt,cmi}
        ocamlc forutop/.utop/utop.exe
  hello in utop
