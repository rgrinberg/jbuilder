  $ $JBUILDER build @lint -j1 --root .
      ocamldep linter.depends.ocamldep-output
        ocamlc linter.{cmi,cmo,cmt}
      ocamlopt linter.{cmx,o}
      ocamlopt linter.exe
        linter alias lint (exit 1)
  (cd _build/default && ./linter.exe linted.ml)
  Linting successfully failed.
  [1]
