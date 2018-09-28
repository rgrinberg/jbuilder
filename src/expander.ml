open Import

type t =
  { ocaml_config : Value.t list String.Map.t
  ; pforms       : Pform.Map.t
  }

let make ~ocaml_config ~pforms =
  { ocaml_config
  ; pforms
  }

module Static = struct
  let expand_ocaml_config t pform name =
    match String.Map.find t.ocaml_config name with
    | Some x -> x
    | None ->
      Errors.fail (String_with_vars.Var.loc pform)
        "Unknown ocaml configuration variable %S"
        name

  let expand_vars t ~mode ~scope ~dir
        ?(bindings=Pform.Map.empty) s =
    String_with_vars.expand ~mode ~dir s ~f:(fun pform syntax_version ->
      (match Pform.Map.expand bindings pform syntax_version with
       | None -> Pform.Map.expand t.pforms pform syntax_version
       | Some _ as x -> x)
      |> Option.map ~f:(function
        | Pform.Expansion.Var (Values l) -> l
        | Macro (Ocaml_config, s) -> expand_ocaml_config t pform s
        | Var Project_root -> [Value.Dir (Scope.root scope)]
        | _ ->
          Errors.fail (String_with_vars.Var.loc pform)
            "%s isn't allowed in this position"
            (String_with_vars.Var.describe pform)))

  let expand_vars_string t ~scope ~dir ?bindings s =
    expand_vars t ~mode:Single ~scope ~dir ?bindings s
    |> Value.to_string ~dir

  let expand_vars_path t ~scope ~dir ?bindings s =
    expand_vars t ~mode:Single ~scope ~dir ?bindings s
    |> Value.to_path ~error_loc:(String_with_vars.loc s) ~dir
end

let expand_and_eval_set t ~scope ~dir ?bindings set ~standard =
  let open Build.O in
  let parse ~loc:_ s = s in
  let (syntax, files) =
    let f = Static.expand_vars_path t ~scope ~dir ?bindings in
    Ordered_set_lang.Unexpanded.files set ~f in
  let f = Static.expand_vars t ~mode:Many ~scope ~dir ?bindings in
  match Path.Set.to_list files with
  | [] ->
    let set =
      Ordered_set_lang.Unexpanded.expand set ~dir
        ~files_contents:Path.Map.empty ~f
    in
    standard >>^ fun standard ->
    Ordered_set_lang.String.eval set ~standard ~parse
  | paths ->
    Build.fanout standard (Build.all (List.map paths ~f:(fun f ->
      Build.read_sexp f syntax)))
    >>^ fun (standard, sexps) ->
    let files_contents = List.combine paths sexps |> Path.Map.of_list_exn in
    let set = Ordered_set_lang.Unexpanded.expand set ~dir ~files_contents ~f in
    Ordered_set_lang.String.eval set ~standard ~parse

module Dynamic = struct
  module Resolved_forms = struct
    type t =
      { (* Failed resolutions *)
        mutable failures  : fail list
      ; (* All "name" for %{lib:name:...}/%{lib-available:name} forms *)
        mutable lib_deps  : Lib_deps_info.t
      ; (* Static deps from %{...} variables. For instance %{exe:...} *)
        mutable sdeps     : Path.Set.t
      ; (* Dynamic deps from %{...} variables. For instance %{read:...} *)
        mutable ddeps     : (unit, Value.t list) Build.t String.Map.t
      }

    let failures t = t.failures
    let lib_deps t = t.lib_deps
    let sdeps t = t.sdeps
    let ddeps t = t.ddeps

    let empty () =
      { failures  = []
      ; lib_deps  = Lib_name.Map.empty
      ; sdeps     = Path.Set.empty
      ; ddeps     = String.Map.empty
      }

    let add_lib_dep acc lib kind =
      acc.lib_deps <- Lib_name.Map.add acc.lib_deps lib kind

    let add_fail acc fail =
      acc.failures <- fail :: acc.failures;
      None

    let add_ddep acc ~key dep =
      acc.ddeps <- String.Map.add acc.ddeps key dep;
      None
  end

  let path_exp path = [Value.Path path]
  let str_exp  str  = [Value.String str]

  let parse_lib_file ~loc s =
    match String.lsplit2 s ~on:':' with
    | None ->
      Errors.fail loc "invalid %%{lib:...} form: %s" s
    | Some (lib, f) -> (Lib_name.of_string_exn ~loc:(Some loc) lib, f)

  open Build.O

  type targets =
    | Static of Path.t list
    | Infer
    | Alias

  let expander ~acc sctx ~dir ~dep_kind ~scope ~targets_written_by_user
        ~map_exe ~bindings pform syntax_version =
    let loc = String_with_vars.Var.loc pform in
    let key = String_with_vars.Var.full_name pform in
    let res =
      Pform.Map.expand bindings pform syntax_version
      |> Option.bind ~f:(function
        | Pform.Expansion.Var (Values l) -> Some l
        | Macro (Ocaml_config, s) ->
          Some (Static.expand_ocaml_config sctx pform s)
        | Var Project_root -> Some [Value.Dir (Scope.root scope)]
        | Var (First_dep | Deps | Named_local) -> None
        | Var Targets ->
          begin match targets_written_by_user with
          | Infer ->
            Errors.fail loc "You cannot use %s with inferred rules."
              (String_with_vars.Var.describe pform)
          | Alias ->
            Errors.fail loc "You cannot use %s in aliases."
              (String_with_vars.Var.describe pform)
          | Static l ->
            Some (Value.L.dirs l) (* XXX hack to signal no dep *)
          end
        | Macro (Exe, s) -> Some (path_exp (map_exe (Path.relative dir s)))
        | Macro (Dep, s) -> Some (path_exp (Path.relative dir s))
        | Macro (Bin, s) -> begin
            let sctx = host sctx in
            match Artifacts.binary ~loc:None (artifacts sctx) s with
            | Ok path -> Some (path_exp path)
            | Error e ->
              Resolved_forms.add_fail acc
                ({ fail = fun () -> Action.Prog.Not_found.raise e })
          end
        | Macro (Lib, s) -> begin
            let lib_dep, file = parse_lib_file ~loc s in
            Resolved_forms.add_lib_dep acc lib_dep dep_kind;
            match
              Artifacts.file_of_lib (artifacts sctx) ~loc ~lib:lib_dep ~file
            with
            | Ok path -> Some (path_exp path)
            | Error fail -> Resolved_forms.add_fail acc fail
          end
        | Macro (Libexec, s) -> begin
            let sctx = host sctx in
            let lib_dep, file = parse_lib_file ~loc s in
            Resolved_forms.add_lib_dep acc lib_dep dep_kind;
            match
              Artifacts.file_of_lib (artifacts sctx) ~loc ~lib:lib_dep ~file
            with
            | Error fail -> Resolved_forms.add_fail acc fail
            | Ok path ->
              if not Sys.win32 || Filename.extension s = ".exe" then begin
                Some (path_exp path)
              end else begin
                let path_exe = Path.extend_basename path ~suffix:".exe" in
                let dep =
                  Build.if_file_exists path_exe
                    ~then_:(Build.path path_exe >>^ fun _ ->
                            path_exp path_exe)
                    ~else_:(Build.path path >>^ fun _ ->
                            path_exp path)
                in
                Resolved_forms.add_ddep acc ~key dep
              end
          end
        | Macro (Lib_available, s) -> begin
            let lib = Lib_name.of_string_exn ~loc:(Some loc) s in
            Resolved_forms.add_lib_dep acc lib Optional;
            Some (str_exp (string_of_bool (
              Lib.DB.available (Scope.libs scope) lib)))
          end
        | Macro (Version, s) -> begin
            match Package.Name.Map.find
                    (Dune_project.packages (Scope.project scope))
                    (Package.Name.of_string s) with
            | Some p ->
              let x =
                Pkg_version.read sctx p >>^ function
                | None   -> [Value.String ""]
                | Some s -> [String s]
              in
              Resolved_forms.add_ddep acc ~key x
            | None ->
              Resolved_forms.add_fail acc { fail = fun () ->
                Errors.fail loc
                  "Package %S doesn't exist in the current project." s
              }
          end
        | Macro (Read, s) -> begin
            let path = Path.relative dir s in
            let data =
              Build.contents path
              >>^ fun s -> [Value.String s]
            in
            Resolved_forms.add_ddep acc ~key data
          end
        | Macro (Read_lines, s) -> begin
            let path = Path.relative dir s in
            let data =
              Build.lines_of path
              >>^ Value.L.strings
            in
            Resolved_forms.add_ddep acc ~key data
          end
        | Macro (Read_strings, s) -> begin
            let path = Path.relative dir s in
            let data =
              Build.strings path
              >>^ Value.L.strings
            in
            Resolved_forms.add_ddep acc ~key data
          end
        | Macro (Path_no_dep, s) -> Some [Value.Dir (Path.relative dir s)])
    in
    Option.iter res ~f:(fun v ->
      acc.sdeps <- Path.Set.union
                     (Path.Set.of_list (Value.L.deps_only v)) acc.sdeps
    );
    res

  let with_expander sctx ~dir ~dep_kind ~scope ~targets_written_by_user
        ~map_exe ~bindings ~f =
    let acc = Resolved_forms.empty () in
    ( f (expander ~acc sctx ~dir ~dep_kind ~scope ~targets_written_by_user ~map_exe ~bindings)
    , acc
    )
end

