open Import

let make_ext_replace (config : Ocaml_config.Vars.t) =
  let tbl =
    List.filter_map [ "ext_exe"; "ext_dll"; "ext_asm"; "ext_lib"; "ext_obj" ]
      ~f:(fun var ->
        match Ocaml_config.Vars.find config var with
        | Some "" -> None
        | Some s -> Some (s, "$" ^ var)
        | None -> (
          match (var, Ocaml_config.Vars.find config "system") with
          | "ext_exe", Some "Win32" -> Some (".exe", var)
          | _ -> None ))
  in
  let re =
    Re.(
      compile
        (seq
           [ diff any (char '/')
           ; alt (List.map tbl ~f:(fun (s, _) -> str s))
           ; eow
           ]))
  in
  let map = String.Map.of_list_reduce tbl ~f:(fun _ x -> x) in
  fun s ->
    Re.replace re s ~f:(fun g ->
        let s = Re.Group.get g 0 in
        sprintf "%c%s" s.[0] (String.Map.find_exn map (String.drop s 1)))

let bleach =
  let f =
    lazy
      ( match Env.get Env.initial "INSIDE_DUNE" with
      | None -> fun s -> s
      | Some build_dir ->
        let build_dir = Path.of_string build_dir in
        let ocaml_config =
          Configurator_config.ocaml_config_from_dune_dir ~build_dir
        in
        make_ext_replace ocaml_config )
  in
  fun x -> Lazy.force f x
