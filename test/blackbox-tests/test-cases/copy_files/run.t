  $ $JBUILDER build -j1 test.exe .merlin --root . --debug-dependency-path
      ocamllex lexers/lexer1.ml
      ocamldep test.depends.ocamldep-output
      ocamldep foo.depends.ocamldep-output
        ocamlc bar.o
        ocamlc .foo.objs/dummy.{cmi,cmo,cmt}
    ocamlmklib dllfoo_stubs.so,libfoo_stubs.a
        ocamlc lexer1.{cmi,cmo,cmt}
      ocamlopt .foo.objs/dummy.{cmx,o}
        ocamlc test.{cmi,cmo,cmt}
      ocamlopt lexer1.{cmx,o}
      ocamlopt foo.{a,cmxa}
      ocamlopt test.{cmx,o}
      ocamlopt test.exe
  $ $JBUILDER build -j1 @bar-source --root .
  #line 1 "include/bar.h"
  int foo () {return 42;}
