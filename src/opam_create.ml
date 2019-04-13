open Stdune

module Mutator = struct
  open OpamParserTypes

  type t = opamfile_item list -> opamfile_item list

  let (>>>) : t -> t -> t = fun x y z -> y (x z)

  let nopos = ("",0,0) (* Null position *)

  let fixup : t = List.map ~f:(function
    | Variable (x,y,String (pos,z)) ->
      let fixed =
        if String.length z > 0 && z.[0] = '\n'
        then String.sub z ~pos:1 ~len:(String.length z - 1)
        else z
      in
      Variable (x,y,String (pos,fixed))
    | y -> y)

  let _remove_var : string -> t =
    fun str -> List.filter ~f:(function
      | Variable (_, v, _) when v=str -> false
      | _ -> true)

  let add_var : string -> OpamParserTypes.value -> t = fun var value l ->
    (Variable (nopos, var, value))::l

  let remap x f =
    List.filter_map ~f:(function
      | Variable (_, v, y) when v = x -> begin
        match f (Some y) with
        | Some y' -> Some (Variable (nopos, v, y'))
        | None -> None
        end
      | z -> Some z)

  let binding_present x =
    List.exists ~f:(function
      | Variable (_, v, _) when v = x -> true
      | _ -> false)

  let _map_var x f zs =
    if binding_present x zs
    then remap x f zs
    else begin
      match f None with
      | Some y -> (Variable (nopos, x, y))::zs
      | None -> zs
    end

  let set_var x y zs =
    if binding_present x zs
    then remap x (fun _ -> Some y) zs
    else add_var x y zs

  let mkstring x = String (nopos, x)
  let mklist f xs = List (nopos, List.map ~f xs)

  let set_string x y = set_var x (mkstring y)

  let set_list x conv l = set_var x (mklist conv l)
  let id x = x

  let opt opt f : t = match opt with | None -> id | Some x -> f x
  let list l f : t = match l with [] -> id | xs -> f xs

  let apply t opamfile =
    {
      opamfile with
      file_contents = t opamfile.file_contents
    }
end

let correct_specific _project package =
  let open Mutator in
  let open Dune_project.Opam_package in
  set_string "synopsis" package.synopsis >>>
  set_string "description" package.description

let correct project package_name =
  let open Mutator in
  opt (Dune_project.opam_package project package_name)
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
        let package_name = Local_package.name pkg |> Package.Name.to_string in
        let corrected = Mutator.apply (correct project package_name) opamfile in
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
