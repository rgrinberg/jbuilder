  $ dune build --display short foo.bc.js
  Don't know how to build foo.bc.js
  [1]

  $ dune clean
  $ dune build --display short bar.bc.js
      ocamldep .bar.eobjs/bar.ml.d
   js_of_ocaml bar.bc.runtime.js
        ocamlc .bar.eobjs/byte/bar.{cmi,cmo,cmt}
   js_of_ocaml .bar.eobjs/byte/bar.cmo.js
   js_of_ocaml .js/stdlib/stdlib.cma.js
     jsoo_link bar.bc.js

  $ dune clean
  $ dune build --display short zzz.cma
      ocamldep .zzz.objs/zzz.ml.d
        ocamlc .zzz.objs/byte/zzz.{cmi,cmo,cmt}
        ocamlc zzz.cma
  $ dune build --display short _build/default/.zzz.objs/zzz.cma.js
   js_of_ocaml .zzz.objs/zzz.cma.js
  $ dune build --display short _build/default/.zzz2.objs/zzz2.cma.js
  Don't know how to build _build/default/.zzz2.objs/zzz2.cma.js
  [1]

  $ dune clean
  $ dune build --display short @all
      ocamldep .bar.eobjs/bar.ml.d
   js_of_ocaml bar.bc.runtime.js
   js_of_ocaml .js/stdlib/stdlib.cma.js
      ocamldep .cha.eobjs/cha.ml.d
        ocamlc .cha.eobjs/byte/cha.{cmi,cmo,cmt}
        ocamlc cha.bc
      ocamldep .foo.eobjs/foo.ml.d
        ocamlc .foo.eobjs/byte/foo.{cmi,cmo,cmt}
        ocamlc foo.bc
      ocamldep .zzz.objs/zzz.ml.d
        ocamlc .zzz.objs/byte/zzz.{cmi,cmo,cmt}
        ocamlc zzz.cma
      ocamldep .zzz2.objs/zzz2.ml.d
        ocamlc .zzz2.objs/byte/zzz2.{cmi,cmo,cmt}
        ocamlc zzz2.cma
        ocamlc .bar.eobjs/byte/bar.{cmi,cmo,cmt}
   js_of_ocaml .bar.eobjs/byte/bar.cmo.js
     jsoo_link bar.bc.js
      ocamlopt .cha.eobjs/native/cha.{cmx,o}
      ocamlopt cha.exe
      ocamlopt .foo.eobjs/native/foo.{cmx,o}
      ocamlopt foo.exe
      ocamlopt .zzz2.objs/native/zzz2.{cmx,o}
      ocamlopt zzz2.{a,cmxa}
      ocamlopt zzz2.cmxs
        ocamlc bar.bc
