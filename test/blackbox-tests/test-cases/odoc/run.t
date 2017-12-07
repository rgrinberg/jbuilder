  $ $JBUILDER build @doc -j1 --root .
      ocamldep foo_byte.depends.ocamldep-output
      ocamldep foo_byte.dependsi.ocamldep-output
      ocamldep foo.depends.ocamldep-output
      ocamldep foo.dependsi.ocamldep-output
          odoc _doc/odoc.css
        ocamlc foo_byte.{cmi,cmo,cmt}
        ocamlc foo.{cmi,cmo,cmt}
          odoc foo_byte.odoc
          odoc foo.odoc
          odoc _doc/foo.byte/index.html
          odoc _doc/foo.byte/Foo_byte/.jbuilder-keep,_doc/foo.byte/Foo_byte/index.html
          odoc _doc/foo/index.html
          odoc _doc/foo/Foo/.jbuilder-keep,_doc/foo/Foo/index.html
  $ $JBUILDER runtest -j1 --root .
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <title>index</title>
      <link rel="stylesheet" href="./odoc.css"/>
      <meta charset="utf-8"/>
      <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    </head>
    <body>
      <div class="by-name">
      <h2>OCaml package documentation</h2>
      <ol>
      <li><a href="foo/index.html">foo</a></li>
      <li><a href="foo.byte/index.html">foo.byte</a></li>
      </ol>
   </body>
   </html>
