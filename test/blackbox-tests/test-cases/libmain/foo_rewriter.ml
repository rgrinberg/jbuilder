let mapper =
  Migrate_parsetree.Ast_405.shallow_identity

let () =
  Migrate_parsetree.Driver.register
    ~name:"foo_rewriter"
    Migrate_parsetree.Versions.ocaml_405
    (fun _config _cookies -> mapper)
