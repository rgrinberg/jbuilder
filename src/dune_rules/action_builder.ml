open Import
include Dune_engine.Action_builder
open O
module With_targets = With_targets

type fail = { fail : 'a. unit -> 'a }

let fail x =
  let+ () = return () in
  x.fail ()
;;

let delayed f =
  let+ () = return () in
  f ()
;;

let of_memo_join f = of_memo f >>= Fun.id

let dyn_memo_deps deps =
  let* deps, a = of_memo deps in
  let+ () = Build_system.record_deps deps in
  a
;;

let deps d = dyn_memo_deps (Memo.return (d, ()))
let dep d = deps (Dep.Set.singleton d)

let dyn_deps t =
  let* a, deps = t in
  let+ () = Build_system.record_deps deps in
  a
;;

let source_dir_deps_if_any path =
  let open Memo.O in
  match Path.extract_build_context_dir path with
  | None -> Memo.return None
  | Some (_ctx_dir, source_dir) when Path.Source.is_root source_dir -> Memo.return None
  | Some (_ctx_dir, source_dir) ->
    Source_tree.find_dir source_dir
    >>= (function
     | None -> Memo.return None
     | Some _ ->
       Load_rules.is_target path
       >>| (function
        | No -> Some (Source_deps.files path)
        | Yes _ | Under_directory_target_so_cannot_say -> None))
;;

let path path =
  of_memo (source_dir_deps_if_any path)
  >>= function
  | None -> deps (Dep.Set.singleton (Dep.file path))
  | Some source_dir_deps ->
    let+ (_ : Path.Set.t) = dyn_memo_deps source_dir_deps in
    ()
;;

let path_list paths = all_unit (Stdune.List.map paths ~f:path)
let paths = path_list
let path_set path_set = path_list (Path.Set.to_list path_set)

let dyn_paths t =
  let* x, paths = t in
  let+ () = path_list paths in
  x
;;

let dyn_paths_unit t = t >>= path_list
let contents p = of_memo (Build_system.read_file p)
let lines_of p = contents p >>| String.split_lines

let read_sexp p =
  let+ s = contents p in
  Dune_sexp.Parser.parse_string s ~fname:(Path.to_string p) ~mode:Single
;;

let with_targets build ~targets : _ With_targets.t = { build; targets }
let with_no_targets build : _ With_targets.t = { build; targets = Targets.empty }

let with_file_targets build ~file_targets : _ With_targets.t =
  { build; targets = Targets.Files.create (Path.Build.Set.of_list file_targets) }
;;

let write_file ?(perm = Action.File_perm.Normal) fn s =
  with_file_targets
    ~file_targets:[ fn ]
    (return (Action.Full.make (Action.Write_file (fn, perm, s))))
;;

let write_file_dyn ?(perm = Action.File_perm.Normal) fn s =
  with_file_targets
    ~file_targets:[ fn ]
    (let+ s = s in
     Action.Full.make (Action.Write_file (fn, perm, s)))
;;

let with_stdout_to ?(perm = Action.File_perm.Normal) fn t =
  with_targets
    ~targets:(Targets.File.create fn)
    (let+ (act : Action.Full.t) = t in
     Action.Full.map act ~f:(Action.with_stdout_to ~perm fn))
;;

let copy ~src ~dst =
  with_file_targets
    ~file_targets:[ dst ]
    (path src >>> return (Action.Full.make (Action.Copy (src, dst))))
;;

let copy_dir ~src ~dst =
  with_targets
    ~targets:
      (Targets.create ~files:Path.Build.Set.empty ~dirs:(Path.Build.Set.singleton dst))
    (path src >>> return (Action.Full.make (Action.Copy (src, dst))))
;;

let symlink ~src ~dst =
  with_file_targets
    ~file_targets:[ dst ]
    (path src >>> return (Action.Full.make (Action.Symlink (src, dst))))
;;

let symlink_dir ~src ~dst =
  with_targets
    ~targets:
      (Targets.create ~files:Path.Build.Set.empty ~dirs:(Path.Build.Set.singleton dst))
    (path src >>> return (Action.Full.make (Action.Symlink (src, dst))))
;;

let progn ts =
  let open With_targets.O in
  With_targets.all ts >>| Action.Full.reduce
;;

let if_file_exists p ~then_ ~else_ =
  let* exists = of_memo (Build_system.file_exists p) in
  if exists then then_ else else_
;;

let file_exists p = if_file_exists p ~then_:(return true) ~else_:(return false)

let paths_existing paths =
  all_unit
    (Stdune.List.map paths ~f:(fun file ->
       if_file_exists file ~then_:(path file) ~else_:(return ())))
;;

let paths_matching g =
  let* filenames = of_memo @@ Build_system.eval_pred g in
  let+ () = Build_system.record_deps (Dep.Set.singleton (Dep.file_selector g)) in
  filenames
;;

let ignore x = map x ~f:ignore

let paths_matching ~loc:_ g =
  (* CR-someday rgrinberg: how about doing something with this location? Like pushing a
     stack frame with it for example *)
  let* () = return () in
  paths_matching g
;;

let paths_matching_unit ~loc g = ignore (paths_matching ~loc g)
let env_var s = deps (Dep.Set.singleton (Dep.env s))
