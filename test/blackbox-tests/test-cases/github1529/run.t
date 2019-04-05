Reproduction case for #1529: using an extension when no dune-project
file is present.

  $ dune build
  Info: creating file dune-project with this contents:
  | (lang dune 1.9)
  | ;; (explicit_js_mode) ;; uncomment to only enable js targets with (modes js)
  | ;; (implicit_transitive_deps false) ;; uncomment to disallow transitive deps
  
  Info: appending this line to dune-project: (using menhir 2.0)
