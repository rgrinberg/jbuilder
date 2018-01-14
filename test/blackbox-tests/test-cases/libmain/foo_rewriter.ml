let mapper =
  let open Migrate_parsetree.Ast_405 in
  let open Parsetree in
  let super = Ast_mapper.default_mapper in
  let expr self e =
    match e.pexp_desc with
    | Pexp_extension ({ txt = "foo"; _ }, PStr []) ->
      { e with pexp_desc = Pexp_constant (Pconst_string ("FOO", None)) }
    | _ -> super.expr self e in
  { super with expr }


let () =
  Migrate_parsetree.Driver.register
    ~name:"foo_rewriter"
    Migrate_parsetree.Versions.ocaml_405
    (fun _config _cookies -> mapper)
