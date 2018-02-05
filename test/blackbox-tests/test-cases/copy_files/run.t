  $ $JBUILDER build -j0 test.exe .merlin --root . --debug-dependency-path
      ocamllex lexers/lexer1.ml
      ocamldep test.depends.ocamldep-output
      ocamldep foo.depends.ocamldep-output
        ocamlc dummy.{cmi,cmo,cmt}
      ocamlopt dummy.{cmx,o}
      ocamlopt foo.{a,cmxa}
        ocamlc lexer1.{cmi,cmo,cmt}
      ocamlopt lexer1.{cmx,o}
        ocamlc bar.o
    ocamlmklib dllfoo_stubs.so,libfoo_stubs.a
        ocamlc test.{cmi,cmo,cmt}
      ocamlopt test.{cmx,o}
      ocamlopt test.exe
  $ $JBUILDER build -j0 @bar-source --root .
  #line 1 "include/bar.h"
  int foo () {return 42;}
