  $ [ -e dune-project ] || echo File does not exist
  File does not exist
  $ mkdir src
  $ echo '(alias (name runtest) (action (progn)))' >  src/dune
  $ dune build
  Info: creating file dune-project with this contents:
  | (lang dune 1.9)
  | ;; (explicit_js_mode) ;; uncomment to only enable js targets with (modes js)
  | ;; (implicit_transitive_deps false) ;; uncomment to disallow transitive deps
  
  $ cat dune-project
  (lang dune 1.9)
  ;; (explicit_js_mode) ;; uncomment to only enable js targets with (modes js)
  ;; (implicit_transitive_deps false) ;; uncomment to disallow transitive deps

Test that using menhir automatically update the dune-project file

  $ echo '(library (name x)) (menhir (modules x))' >> src/dune
  $ dune build
  Info: appending this line to dune-project: (using menhir 2.0)
  $ cat dune-project
  (lang dune 1.9)
  ;; (explicit_js_mode) ;; uncomment to only enable js targets with (modes js)
  ;; (implicit_transitive_deps false) ;; uncomment to disallow transitive deps
  (using menhir 2.0)
