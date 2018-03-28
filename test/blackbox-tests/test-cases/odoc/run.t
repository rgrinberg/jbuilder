  $ $JBUILDER build @doc -j1 --display short --root .
          odoc _doc/_odoc/pkg/bar/page-index.odoc
          odoc _doc/_html/bar/index.html
          odoc _doc/_html/odoc.css
      ocamldep foo.ml.d
        ocamlc .foo.objs/foo.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/foo/foo.odoc
      ocamldep foo_byte.ml.d
        ocamlc .foo_byte.objs/foo_byte.{cmi,cmo,cmt}
          odoc _doc/_odoc/lib/foo.byte/foo_byte.odoc
          odoc _doc/_html/foo/Foo/.jbuilder-keep,_doc/_html/foo/Foo/index.html
          odoc _doc/_odoc/pkg/foo/page-index.odoc
          odoc _doc/_html/foo/index.html
          odoc _doc/_html/foo/Foo_byte/.jbuilder-keep,_doc/_html/foo/Foo_byte/index.html

  $ $JBUILDER build @package-listing -j1 --display short --root .
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
      <li><a href="bar/index.html">bar</a></li>
      <li><a href="foo/index.html">foo</a></li>
      </ol>
      </div>
    </body>
  </html>

  $ $JBUILDER build @foo-module-listing -j1 --display short --root .
          odoc _doc/_html/foo/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>index (foo.index)</title><link rel="stylesheet" href="../odoc.css"/><meta charset="utf-8"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/><meta name="generator" content="doc-ock-html v1.0.0-1-g1fc9bf0"/></head><body><nav id="top"><a href="../index.html">Up</a> &mdash; <span class="package">package <a href="index.html">foo</a></span></nav><header></header><h2>Library foo</h2><p>
  This library exposes the following toplevel modules:
  </p><table class="modules"><tr id="listing-module-Foo" class="anchored"><td class="module"><a href="#listing-module-Foo" class="anchor"></a><a href="Foo/index.html">Foo</a></td><td class="doc"></td></tr></table><h2>Library foo.byte</h2><p>
  This library exposes the following toplevel modules:
  </p><table class="modules"><tr id="listing-module-Foo_byte" class="anchored"><td class="module"><a href="#listing-module-Foo_byte" class="anchor"></a><a href="Foo_byte/index.html">Foo_byte</a></td><td class="doc"></td></tr></table></body></html>
