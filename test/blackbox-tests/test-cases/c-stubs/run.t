  $ $JBUILDER exec -j0 ./qnativerun/run.exe --root .
      ocamldep qnativerun/run.depends.ocamldep-output
        ocamlc q/q_stub.o
    ocamlmklib q/dllq_stubs.so,q/libq_stubs.a
      ocamldep q/q.depends.ocamldep-output
      ocamldep q/q.dependsi.ocamldep-output
        ocamlc q/q.{cmi,cmti}
      ocamlopt q/q.{cmx,o}
      ocamlopt q/q.{a,cmxa}
        ocamlc qnativerun/run.{cmi,cmo,cmt}
      ocamlopt qnativerun/run.{cmx,o}
      ocamlopt qnativerun/run.exe
  42
#  $ $JBUILDER exec -j0 ./qbyterun/run.bc --root .
