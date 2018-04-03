  $ jbuilder runtest --root . --display quiet -j1
          main alias runtest
  hello
  $ echo 'let x = 1' >> lib_sub.ml
  $ jbuilder runtest --root . --display quiet -j1 2>&1 | grep -v ocamlopt
  File "_none_", line 1:
  Error: Files .main.eobjs/main.cmx and .main.eobjs/lib_sub.cmx
         make inconsistent assumptions over implementation Lib_sub
