let () =
  Migrate_parsetree.Driver.register
    ~name:"linter"
    Migrate_parsetree.Versions.ocaml_405
    (fun _ cookies ->
       Format.eprintf "Linting failed@.%!";
       exit 1
    )
