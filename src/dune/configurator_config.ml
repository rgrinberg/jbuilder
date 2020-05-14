open Import

let dot_dune_dir build_dir = Path.relative build_dir ".dune"

let configurator_v2 dot_dune_dir = Path.relative dot_dune_dir "configurator.v2"

let dune_keep_fname = ".dune-keep"

(* We store this so that library such as dune-configurator can read things
   runtime. Ideally, this should be created on-demand if we run a program linked
   against configurator, however we currently don't support this kind of
   "runtime dependencies" so we just do it eagerly. *)
let write_dot_dune_dir ~build_dir ~ocamlc ~ocaml_config_vars =
  let dir = dot_dune_dir (Path.build build_dir) in
  Path.rm_rf dir;
  Path.mkdir_p dir;
  Io.write_file (Path.relative dir dune_keep_fname) "";
  let ocamlc = Path.to_absolute_filename ocamlc in
  let ocaml_config_vars = Ocaml_config.Vars.to_list ocaml_config_vars in
  let () =
    let open Dune_lang.Encoder in
    Io.write_lines
      (Path.relative dir "configurator")
      (List.map ~f:Dune_lang.to_string
         (record_fields
            [ field "ocamlc" string ocamlc
            ; field_l "ocaml_config_vars" (pair string string) ocaml_config_vars
            ]))
  in
  let () =
    let csexp =
      let open Sexp in
      let ocaml_config_vars =
        Sexp.List
          (List.map ocaml_config_vars ~f:(fun (k, v) -> List [ Atom k; Atom v ]))
      in
      List
        [ List [ Atom "ocamlc"; Atom ocamlc ]
        ; List [ Atom "ocaml_config_vars"; ocaml_config_vars ]
        ]
    in
    let path = configurator_v2 dir in
    Io.write_file path (Csexp.to_string csexp)
  in
  ()

let ocaml_config_from_dune_dir ~build_dir =
  let dir = dot_dune_dir build_dir in
  let file = configurator_v2 dir in
  let open Sexp in
  let unable_to_parse err =
    User_error.raise
      [ Pp.textf "Unable to parse %S." (Path.to_string_maybe_quoted file)
      ; Pp.text err
      ]
  in
  let sexp =
    match Io.with_file_in file ~f:Csexp.input with
    | Ok s -> s
    | Error e -> unable_to_parse e
  in
  match sexp with
  | Atom _ -> unable_to_parse "unexpected atom"
  | List xs ->
    let field name =
      match
        List.find_map xs ~f:(function
          | List [ Atom name'; f ] when name = name' -> Some f
          | _ -> None)
      with
      | Some f -> f
      | None -> unable_to_parse ("unable to find field " ^ name)
    in
    let bindings =
      match field "ocaml_config_vars" with
      | List bindings ->
        List.map bindings ~f:(function
          | List [ Atom k; Atom v ] -> (k, v)
          | _ -> unable_to_parse "invalid output")
      | _ -> unable_to_parse "invalid output"
    in
    Ocaml_config.Vars.of_list_exn bindings
