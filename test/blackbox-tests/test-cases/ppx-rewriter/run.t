  $ dune build ./w_omp_driver.exe --display short
      ocamldep ppx/fooppx.ml.d
        ocamlc ppx/.fooppx.objs/fooppx.{cmi,cmo,cmt}
      ocamlopt ppx/.fooppx.objs/fooppx.{cmx,o}
      ocamlopt ppx/fooppx.{a,cmxa}
      ocamlopt .ppx/fooppx@./ppx.exe
           ppx w_omp_driver.pp.ml
      ocamldep w_omp_driver.pp.ml.d
        ocamlc .w_omp_driver.eobjs/w_omp_driver.{cmi,cmo,cmt}
      ocamlopt .w_omp_driver.eobjs/w_omp_driver.{cmx,o}
      ocamlopt w_omp_driver.exe
  $ dune build ./w_ppx_driver.exe --display short
      ocamlopt .ppx/ppx_driver.runner/ppx.exe
           ppx w_ppx_driver.pp.ml
      ocamldep w_ppx_driver.pp.ml.d
        ocamlc .w_ppx_driver.eobjs/w_ppx_driver.{cmi,cmo,cmt}
      ocamlopt .w_ppx_driver.eobjs/w_ppx_driver.{cmx,o}
      ocamlopt w_ppx_driver.exe
This test is broken because ppx_driver doesn't support migrate custom arguments
#  $ dune build ./w_ppx_driver_flags.exe --display short
