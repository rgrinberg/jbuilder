  $ $JBUILDER build -j1 test.exe .merlin --display short --root . --debug-dependency-path
      ocamllex lexers/lexer1.ml
      ocamldep lexer1.ml.d
      ocamldep test.ml.d
      ocamldep dummy.ml.d
        ocamlc .foo.objs/dummy.{cmi,cmo,cmt}
      ocamlopt .foo.objs/dummy.{cmx,o}
      ocamlopt foo.{a,cmxa}
        ocamlc bar.o
    ocamlmklib dllfoo_stubs.so,libfoo_stubs.a
        ocamlc .test.eobjs/lexer1.{cmi,cmo,cmt}
      ocamlopt .test.eobjs/lexer1.{cmx,o}
        ocamlc .test.eobjs/test.{cmi,cmo,cmt}
      ocamlopt .test.eobjs/test.{cmx,o}
      ocamlopt test.exe
  $ $JBUILDER build -j1 @bar-source --display short --root .
  #line 1 "include/bar.h"
  int foo () {return 42;}
