open Import
open Jbuild

type scope =
  { mutable libs : Lib.Internal.t String_map.t
  ; scope        : Scope.t
  }

type t =
  { findlib                  : Findlib.t
  ; (* This include both libraries from the current workspace and external ones *)
    by_public_name           : (string, Lib.t) Hashtbl.t
  ; (* This is to implement the scoping described in the manual *)
    by_internal_name         : (Path.t, scope) Hashtbl.t
  ; (* This is to filter out libraries that are not installable because of missing
       dependencies *)
    instalable_internal_libs : Lib.Internal.t String_map.t
  ; local_public_libs        : Path.t String_map.t
  }

let local_public_libs t = t.local_public_libs

let rec internal_name_scope t ~dir =
  match Hashtbl.find t.by_internal_name dir with
  | Some scope -> scope
  | None ->
    (* [create] ensures that [Hashtbl.find t.by_internal_name Path.root] is [Some _] so
       this [Path.parent dir] is never called with [Path.root] *)
    let scope = internal_name_scope t ~dir:(Path.parent dir) in
    Hashtbl.add t.by_internal_name ~key:dir ~data:scope;
    scope

let find_by_internal_name t ~from name =
  let scope = internal_name_scope t ~dir:from in
  if name = "caml" then (
    Format.eprintf "trying to find caml in: %a@.%!" Path.pp from;
    Format.eprintf "Using scope with libs:@.";
    scope.libs |> String_map.iter ~f:(fun ~key ~data:_ ->
      Format.eprintf "%s " key
    );
    Format.eprintf "@."
  );
  String_map.find name scope.libs

let find_exn t ~from name =
  match find_by_internal_name t ~from name with
  | Some x -> Lib.Internal x
  | None ->
    if name = "caml" then (
      failwith "we found caml in findlib which is wrong af"
    );
    Hashtbl.find_or_add t.by_public_name name
      ~f:(fun name ->
        External (Findlib.find_exn t.findlib name
                    ~required_by:[Utils.jbuild_name_in ~dir:from]))

let find t ~from name =
  match find_exn t ~from name with
  | exception _ -> None
  | x -> Some x

let find_internal t ~from name =
  match find_by_internal_name t ~from name with
  | Some _ as some -> some
  | None ->
    match Hashtbl.find t.by_public_name name with
    | Some (Internal x) -> Some x
    | _ -> None

module Local_closure = Top_closure.Make(String)(struct
    type graph = t
    type t = Lib.Internal.t
    let key ((_, lib) : t) = lib.name
    let deps ((dir, lib) : Lib.Internal.t) graph =
      List.concat_map lib.buildable.libraries ~f:(fun dep ->
        List.filter_map (Lib_dep.to_lib_names dep) ~f:(find_internal ~from:dir graph)) @
      List.filter_map lib.ppx_runtime_libraries ~f:(fun dep ->
        find_internal ~from:dir graph dep)
  end)

let top_sort_internals t ~internal_libraries =
  match Local_closure.top_closure t internal_libraries with
  | Ok l -> l
  | Error cycle ->
    die "dependency cycle between libraries:\n   %s"
      (List.map cycle ~f:(fun lib -> Lib.describe (Internal lib))
       |> String.concat ~sep:"\n-> ")

let lib_is_available t ~from name =
  match find_internal t ~from name with
  | Some (_, lib) -> String_map.mem lib.name t.instalable_internal_libs
  | None -> Findlib.available t.findlib name ~required_by:[Utils.jbuild_name_in ~dir:from]

let choice_is_possible t ~from { Lib_dep.required; forbidden; _ } =
  String_set.for_all required  ~f:(fun name ->      lib_is_available t ~from name ) &&
  String_set.for_all forbidden ~f:(fun name -> not (lib_is_available t ~from name))

let dep_is_available t ~from dep =
  match (dep : Lib_dep.t) with
  | Direct s -> lib_is_available t ~from s
  | Select { choices; _ } -> List.exists choices ~f:(choice_is_possible t ~from)

let compute_instalable_internal_libs t ~internal_libraries =
  List.fold_left (top_sort_internals t ~internal_libraries) ~init:t
    ~f:(fun t (dir, lib) ->
      if not lib.Library.optional ||
         (List.for_all (Library.all_lib_deps lib) ~f:(dep_is_available t ~from:dir) &&
          List.for_all lib.ppx_runtime_libraries  ~f:(lib_is_available t ~from:dir))
      then
        { t with
          instalable_internal_libs =
            String_map.add t.instalable_internal_libs
              ~key:lib.name ~data:(dir, lib)
        }
      else
        t)

let create findlib ~scopes internal_libraries =
  let local_public_libs =
    List.fold_left internal_libraries ~init:String_map.empty ~f:(fun acc (dir, lib) ->
      match lib.Library.public with
      | None -> acc
      | Some { name; _ } -> String_map.add acc ~key:name ~data:dir)
  in
  let t =
    { findlib
    ; by_public_name   = Hashtbl.create 1024
    ; by_internal_name = Hashtbl.create 1024
    ; instalable_internal_libs = String_map.empty
    ; local_public_libs
    }
  in
  (* Initializes the scopes, including [Path.root] so that when there are no <pkg>.opam
     files in parent directories, the scope is the whole workspace. *)
  List.iter scopes ~f:(fun (scope : Scope.t) ->
    Hashtbl.add t.by_internal_name ~key:scope.root
      ~data:{ libs = String_map.empty
            ; scope
            });
  List.iter internal_libraries ~f:(fun ((dir, lib) as internal) ->
    let scope = internal_name_scope t ~dir in
    scope.libs <- String_map.add scope.libs ~key:lib.Library.name ~data:internal;
    Option.iter lib.public ~f:(fun { name; _ } ->
      match Hashtbl.find t.by_public_name name with
      | None
      | Some (External _) ->
        Hashtbl.add t.by_public_name ~key:name ~data:(Internal internal)
      | Some (Internal dup) ->
        let internal_path (path, _) = Path.relative path "jbuild" in
        die "Libraries with identical public names %s defined in %a and %a."
          name Path.pp (internal_path internal) Path.pp (internal_path dup)
    ));
  compute_instalable_internal_libs t ~internal_libraries

let internal_libs_without_non_installable_optional_ones t =
  String_map.values t.instalable_internal_libs

let interpret_lib_dep t ~dir lib_dep =
  match lib_dep with
  | Lib_dep.Direct name -> begin
      match find_exn t ~from:dir name with
      | x -> Inl [x]
      | exception _ ->
        (* Call [find] again to get a proper backtrace *)
        Inr { fail = fun () ->
          ignore (find_exn t ~from:dir name : Lib.t);
          assert false }
    end
  | Select { choices; loc; _ } ->
    match
      List.find_map choices ~f:(fun { required; forbidden; _ } ->
        if String_set.exists forbidden ~f:(lib_is_available t ~from:dir) then
          None
        else
          match
            List.map (String_set.elements required) ~f:(find_exn t ~from:dir)
          with
          | l           -> Some l
          | exception _ -> None)
    with
    | Some l -> Inl l
    | None ->
      Inr { fail = fun () ->
        Loc.fail loc "No solution found for this select form"
      }

let interpret_lib_deps t ~dir lib_deps =
  let libs, failures =
    List.partition_map lib_deps ~f:(interpret_lib_dep t ~dir)
  in
  let internals, externals =
    List.partition_map (List.concat libs) ~f:(function
      | Internal x -> Inl x
      | External x -> Inr x)
  in
  (internals, externals,
   match failures with
   | [] -> None
   | f :: _ -> Some f)

let best_lib_dep_names_exn t ~dir lib_deps =
  List.concat_map lib_deps ~f:(fun lib_dep ->
    match interpret_lib_dep t ~dir lib_dep with
    | Inl libs -> List.map libs ~f:Lib.best_name
    | Inr fail -> fail.fail ())

type resolved_select =
  { src_fn : string
  ; dst_fn : string
  }

let resolve_selects t ~from lib_deps =
  List.filter_map lib_deps ~f:(function
    | Lib_dep.Direct _ -> None
    | Select { result_fn; choices; _ } ->
      let src_fn =
        match List.find choices ~f:(choice_is_possible t ~from) with
        | Some c -> c.file
        | None -> "no solution found"
      in
      Some { dst_fn = result_fn; src_fn })

let unique_library_name t (lib : Lib.t) =
  match lib with
  | External pkg -> pkg.name
  | Internal (dir, lib) ->
    match lib.public with
    | Some x -> x.name
    | None ->
      let scope = internal_name_scope t ~dir in
      match scope.scope.name with
      | None -> lib.name ^ "@"
      | Some s -> lib.name ^ "@" ^ s
