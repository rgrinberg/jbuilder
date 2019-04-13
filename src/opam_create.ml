open Stdune

let correct_specific _project package =
  let open Opam_file.Mutator in
  let open Dune_project.Opam.Package in
  set_string "synopsis" package.synopsis >>>
  set_string "description" package.description

let correct project package_name =
  let open Opam_file.Mutator in
  opt (
    let open Option.O in
    let* opam = Dune_project.opam project in
    Dune_project.Opam.find opam package_name)
    (correct_specific project) >>>
  opt (Dune_project.license project) (set_string "license") >>>
  list (Dune_project.authors project) (set_list "authors" mkstring) >>>
  opt (Dune_project.version project) (set_string "version") >>>
  opt (Option.map ~f:(Format.asprintf "%a" Dune_project.Source_kind.pp)
         (Dune_project.source project)) (set_string "dev-repo") >>>
  fixup

let add_rules sctx ~dir ~project =
  let open Build.O in
  Local_package.defined_in sctx ~dir
  |> List.iter ~f:(fun pkg ->
    let opam_path = Local_package.opam_file pkg in
    let expected_path = Path.extend_basename opam_path ~suffix:".expected" in
    let expected_rule =
      Build.contents opam_path >>^ (fun contents ->
        let opamfile = Lexing.from_string contents |> Opam_file.parse in
        let package_name = Local_package.name pkg in
        let corrected =
          Opam_file.Mutator.apply (correct project package_name) opamfile in
        OpamPrinter.opamfile corrected) >>>
      Build.write_file_dyn expected_path
    in
    let diff_rule =
      Build.paths [expected_path; opam_path]
      >>^ fun () ->
      Action.Diff { Action.Diff.
                    file1 = opam_path
                  ; file2 = expected_path
                  ; optional = false
                  ; mode = Text
                  }
    in
    let dir = Local_package.build_dir pkg in
    let alias = Alias.install ~dir in
    Super_context.add_rule sctx ~dir expected_rule;
    Super_context.add_alias_action sctx alias
      ~dir ~loc:None ~stamp:("opam_diff", opam_path) diff_rule)

let add_rules sctx ~dir =
  let scope = Super_context.find_scope_by_dir sctx dir in
  let project = Scope.project scope in
  if Dune_project.gen_opam_file project then
    add_rules sctx ~dir ~project
