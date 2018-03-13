  $ $JBUILDER build @doc -j1 --display short --root .
      ocamldep foo_byte.ml.d
        ocamlc .foo_byte.objs/foo_byte.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/foo.byte/foo_byte.odoc
      ocamldep foo.ml.d
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/foo/foo.odoc
          odoc _doc/_html/foo.byte/Foo_byte/.jbuilder-keep,_doc/_html/foo.byte/Foo_byte/index.html
          odoc _doc/_html/odoc.css
          odoc _doc/_odoc/pkg/foo/page-index.odoc
          odoc _doc/_html/foo/index.html
          odoc _doc/_html/foo/Foo/.jbuilder-keep,_doc/_html/foo/Foo/index.html
  $ $JBUILDER runtest -j1 --display short --root .
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
