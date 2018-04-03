  $ jbuilder runtest --root . --display quiet -j1
          main alias runtest
  hello
  $ echo 'let x = 1' >> lib_sub.ml
  $ jbuilder runtest --root . --display quiet -j1 2>&1 | grep -v ocamlopt
          main alias runtest
  hello
