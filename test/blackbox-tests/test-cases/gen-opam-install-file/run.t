  $ $JBUILDER runtest -j1 --root .
      ocamldep bar.depends.ocamldep-output
      ocamldep foo_byte.depends.ocamldep-output
      ocamldep foo.depends.ocamldep-output
        ocamlc .foo_byte.objs/foo_byte.{cmi,cmo,cmt}
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
        ocamlc foo_byte.cma
      ocamlopt .foo.objs/foo.{cmx,o}
        ocamlc bar.{cmi,cmo,cmt}
        ocamlc foo.cma
      ocamlopt foo.{a,cmxa}
      ocamlopt bar.{cmx,o}
      ocamlopt foo.cmxs
      ocamlopt bar.exe
  lib: [
    "_build/install/default/lib/foo/META" {"META"}
    "_build/install/default/lib/foo/opam" {"opam"}
    "_build/install/default/lib/foo/foo.cmi"
    "_build/install/default/lib/foo/foo.cmx"
    "_build/install/default/lib/foo/foo.cmt"
    "_build/install/default/lib/foo/foo.ml"
    "_build/install/default/lib/foo/foo.cma"
    "_build/install/default/lib/foo/foo.cmxa"
    "_build/install/default/lib/foo/foo.a"
    "_build/install/default/lib/foo/foo.cmxs"
    "_build/install/default/lib/foo/cfoo.h"
    "_build/install/default/lib/foo/byte/foo_byte.cmi" {"byte/foo_byte.cmi"}
    "_build/install/default/lib/foo/byte/foo_byte.cmt" {"byte/foo_byte.cmt"}
    "_build/install/default/lib/foo/byte/foo_byte.ml" {"byte/foo_byte.ml"}
    "_build/install/default/lib/foo/byte/foo_byte.cma" {"byte/foo_byte.cma"}
  ]
  bin: [
    "_build/install/default/bin/bar" {"bar"}
  ]
  share: [
    "_build/install/default/share/foo/bar.ml"
    "_build/install/default/share/foo/baz.ml" {"baz.ml"}
  ]
