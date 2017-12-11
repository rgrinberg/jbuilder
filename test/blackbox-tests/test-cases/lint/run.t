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
      ocamldep linterppx.depends.ocamldep-output
        ocamlc linterppx.{cmi,cmo,cmt}
      ocamlopt linterppx.{cmx,o}
      ocamlopt linterppx.{a,cmxa}
      ocamlopt .ppx/linterppx/ppx.exe
           ppx alias exe/lint (exit 1)
  (cd _build/default && ./.ppx/linterppx/ppx.exe --impl exe/lintedbin.ml)
  Linting failed
  [1]
  $ $JBUILDER build @exe-pass/lint -j1 --root .
        linter alias exe-pass/lint
