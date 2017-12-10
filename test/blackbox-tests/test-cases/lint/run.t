  $ $JBUILDER build @lib/lint -j1 --root .
      ocamldep linter.depends.ocamldep-output
        ocamlc linter.{cmi,cmo,cmt}
      ocamlopt linter.{cmx,o}
      ocamlopt linter.exe
        linter alias lib/lint (exit 1)
  (cd _build/default && ./linter.exe lib/linted.ml)
  Linting lib/linted.ml successfully failed.
  [1]
  $ $JBUILDER build @exe/lint -j1 --root .
        linter alias exe/lint (exit 1)
  (cd _build/default && ./linter.exe exe/lintedbin.ml)
  Linting exe/lintedbin.ml successfully failed.
  [1]
