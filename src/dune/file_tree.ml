open! Stdune
open Import

module File = struct
  type t =
    { ino : int
    ; dev : int
    }

  let to_dyn { ino; dev } =
    let open Dyn.Encoder in
    record [ ("ino", Int.to_dyn ino); ("dev", Int.to_dyn dev) ]

  let compare a b =
    match Int.compare a.ino b.ino with
    | Eq -> Int.compare a.dev b.dev
    | ne -> ne

  let dummy = { ino = 0; dev = 0 }

  let of_stats (st : Unix.stats) = { ino = st.st_ino; dev = st.st_dev }

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare

    let to_dyn _ = Dyn.opaque
  end)
end

module Dune_file = struct
  module Plain = struct
    type t =
      { path : Path.Source.t
      ; mutable sexps : Dune_lang.Ast.t list
      }
  end

  let fname = "dune"

  let jbuild_fname = "jbuild"

  type t =
    | Plain of Plain.t
    | Ocaml_script of Path.Source.t

  let path = function
    | Plain x -> x.path
    | Ocaml_script p -> p

  let load file ~project =
    Io.with_lexbuf_from_file (Path.source file) ~f:(fun lb ->
        let t, sub_dirs =
          if Dune_lexer.is_script lb then
            (Ocaml_script file, Sub_dirs.default)
          else
            let sexps = Dune_lang.Parser.parse lb ~mode:Many in
            let decoder =
              Dune_project.set_parsing_context project Sub_dirs.decode
            in
            let sub_dirs, sexps =
              Dune_lang.Decoder.parse decoder Univ_map.empty
                (Dune_lang.Ast.List (Loc.none, sexps))
            in
            (Plain { path = file; sexps }, sub_dirs)
        in
        (t, sub_dirs))
end

module Readdir : sig
  type t = private
    { files : String.Set.t
    ; dirs : (string * Path.Source.t * File.t) list
    }

  val to_dyn : t -> Dyn.t

  val of_source_path : Path.Source.t -> (t, Unix.error) Result.t
end = struct
  type t =
    { files : String.Set.t
    ; dirs : (string * Path.Source.t * File.t) list
    }

  let to_dyn { files; dirs } =
    let open Dyn.Encoder in
    record
      [ ("files", String.Set.to_dyn files)
      ; ("dirs", list (triple string Path.Source.to_dyn File.to_dyn) dirs)
      ]

  let is_temp_file fn =
    String.is_prefix fn ~prefix:".#"
    || String.is_suffix fn ~suffix:".swp"
    || String.is_suffix fn ~suffix:"~"

  let of_source_path path =
    match Path.readdir_unsorted (Path.source path) with
    | Error unix_error ->
      User_warning.emit
        [ Pp.textf "Unable to read directory %s. Ignoring."
            (Path.Source.to_string_maybe_quoted path)
        ; Pp.text "Remove this message by ignoring by adding:"
        ; Pp.textf "(dirs \\ %s)" (Path.Source.basename path)
        ; Pp.textf "to the dune file: %s"
            (Path.Source.to_string_maybe_quoted
               (Path.Source.relative
                  (Path.Source.parent_exn path)
                  Dune_file.fname))
        ; Pp.textf "Reason: %s" (Unix.error_message unix_error)
        ];
      Error unix_error
    | Ok unsorted_contents ->
      let files, dirs =
        List.filter_partition_map unsorted_contents ~f:(fun fn ->
            let path = Path.Source.relative path fn in
            if Path.Source.is_in_build_dir path then
              Skip
            else
              let is_directory, file =
                match Path.stat (Path.source path) with
                | exception _ -> (false, File.dummy)
                | { st_kind = S_DIR; _ } as st -> (true, File.of_stats st)
                | _ -> (false, File.dummy)
              in
              if is_directory then
                Right (fn, path, file)
              else if is_temp_file fn then
                Skip
              else
                Left fn)
      in
      { files = String.Set.of_list files
      ; dirs =
          List.sort dirs ~compare:(fun (a, _, _) (b, _, _) ->
              String.compare a b)
      }
      |> Result.ok
end

module Dir0 = struct
  module Contents = struct
    type t =
      { files : String.Set.t
      ; sub_dirs : (Sub_dirs.Status.t * Readdir.t) String.Map.t
      ; dune_file : Dune_file.t option
      }

    let create ~files ~sub_dirs ~dune_file = { files; sub_dirs; dune_file }

    let to_dyn { files; sub_dirs; dune_file } =
      let open Dyn.Encoder in
      record
        [ ("files", String.Set.to_dyn files)
        ; ( "sub_dirs"
          , String.Map.to_dyn
              (pair Sub_dirs.Status.to_dyn Readdir.to_dyn)
              sub_dirs )
        ; ("dune_file", Dyn.Encoder.(option opaque dune_file))
        ; ("project", Dyn.opaque)
        ]
  end

  type t =
    { path : Path.Source.t
    ; status : Sub_dirs.Status.t
    ; contents : Contents.t
    ; project : Dune_project.t
    ; vcs : Vcs.t option
    }

  let create ~project ~path ~status ~contents ~vcs =
    { path; status; contents; project; vcs }

  let contents t = t.contents

  let path t = t.path

  let ignored t = t.status = Data_only

  let vendored t = t.status = Vendored

  let files t = (contents t).files

  let sub_dirs t = (contents t).sub_dirs

  let dune_file t = (contents t).dune_file

  let project t = t.project

  let vcs t = t.vcs

  let file_paths t =
    Path.Source.Set.of_listing ~dir:t.path
      ~filenames:(String.Set.to_list (files t))

  let sub_dir_names t =
    String.Map.foldi (sub_dirs t) ~init:String.Set.empty ~f:(fun s _ acc ->
        String.Set.add acc s)

  let sub_dir_paths t =
    String.Map.foldi (sub_dirs t) ~init:Path.Source.Set.empty
      ~f:(fun s _ acc ->
        Path.Source.Set.add acc (Path.Source.relative t.path s))

  let to_dyn { path; status; contents; project = _; vcs } =
    let open Dyn in
    Record
      [ ("path", Path.Source.to_dyn path)
      ; ("status", Sub_dirs.Status.to_dyn status)
      ; ("contents", Contents.to_dyn contents)
      ; ("vcs", Dyn.Encoder.option Vcs.to_dyn vcs)
      ]
end

module Settings : sig
  type t =
    { root : Path.Source.t
    ; ancestor_vcs : Vcs.t option
    ; recognize_jbuilder_projects : bool
    }

  val set : t -> unit

  val get : unit -> t
end = struct
  type t =
    { root : Path.Source.t
    ; ancestor_vcs : Vcs.t option
    ; recognize_jbuilder_projects : bool
    }

  let to_dyn { root; ancestor_vcs; recognize_jbuilder_projects } =
    let open Dyn.Encoder in
    record
      [ ("root", Path.Source.to_dyn root)
      ; ("ancestor_vcs", option Vcs.to_dyn ancestor_vcs)
      ; ("recognize_jbuilder_projects", bool recognize_jbuilder_projects)
      ]

  let t = Fdecl.create to_dyn

  let set = Fdecl.set t

  let get () =
    let (_ : Memo.Run.t) = Memo.current_run () in
    Fdecl.get t
end

let init root ~ancestor_vcs ~recognize_jbuilder_projects =
  Settings.set { Settings.root; ancestor_vcs; recognize_jbuilder_projects }

module rec Memoized : sig
  val root : unit -> Dir0.t

  val find_dir : Path.Source.t -> Dir0.t option
end = struct
  (* This function will read the contents of sub directories. This is because
     we want to exclude directory names that are unreadable from being
     processed by callers *)
  let get_sub_dirs ~dirs ~sub_dirs ~(dir_status : Sub_dirs.Status.t) =
    let sub_dirs =
      Sub_dirs.eval sub_dirs ~dirs:(List.map ~f:(fun (a, _, _) -> a) dirs)
    in
    dirs
    |> List.fold_left ~init:String.Map.empty ~f:(fun acc (fn, path, _file) ->
           let status =
             if Bootstrap.data_only_path path then
               Sub_dirs.Status.Or_ignored.Ignored
             else
               Sub_dirs.status sub_dirs ~dir:fn
           in
           match status with
           | Ignored -> acc
           | Status status -> (
             let dir_status : Sub_dirs.Status.t =
               match (dir_status, status) with
               | Data_only, _ -> Data_only
               | Vendored, Normal -> Vendored
               | _, _ -> status
             in
             match Readdir.of_source_path path with
             | Ok dir -> String.Map.set acc fn (dir_status, dir)
             | Error _ -> acc ))

  let contents { Readdir.dirs; files } ~project ~path
      ~(dir_status : Sub_dirs.Status.t) =
    let recognize_jbuilder_projects =
      let settings = Settings.get () in
      settings.recognize_jbuilder_projects
    in
    let dune_file, sub_dirs =
      if dir_status = Data_only then
        (None, Sub_dirs.default)
      else if
        (not recognize_jbuilder_projects)
        && String.Set.mem files Dune_file.jbuild_fname
      then
        User_error.raise
          ~loc:
            (Loc.in_file
               (Path.source (Path.Source.relative path Dune_file.jbuild_fname)))
          [ Pp.text
              "jbuild files are no longer supported, please convert this file \
               to a dune file instead."
          ; Pp.text
              "Note: You can use \"dune upgrade\" to convert your project to \
               dune."
          ]
      else if not (String.Set.mem files Dune_file.fname) then
        (None, Sub_dirs.default)
      else (
        ignore
          ( Dune_project.ensure_project_file_exists project
            : Dune_project.created_or_already_exist );
        let file = Path.Source.relative path Dune_file.fname in
        let dune_file, sub_dirs = Dune_file.load file ~project in
        (Some dune_file, sub_dirs)
      )
    in
    let sub_dirs = get_sub_dirs ~dirs ~sub_dirs ~dir_status in
    Dir0.Contents.create ~files ~sub_dirs ~dune_file

  let root =
    let f () =
      let settings = Settings.get () in
      let path = settings.root in
      let dir_status : Sub_dirs.Status.t = Normal in
      let readdir =
        match Readdir.of_source_path path with
        | Ok dir -> dir
        | Error m ->
          User_error.raise
            [ Pp.textf "Unable to load source %s.@.Reason:%s@."
                (Path.Source.to_string_maybe_quoted path)
                (Unix.error_message m)
            ]
      in
      let project =
        match
          Dune_project.load ~dir:path ~files:readdir.files
            ~infer_from_opam_files:true
        with
        | None -> Dune_project.anonymous ~dir:path
        | Some p -> p
      in
      let vcs = settings.ancestor_vcs in
      let contents = contents readdir ~project ~path ~dir_status in
      Dir0.create ~project ~path ~status:dir_status ~contents ~vcs
    in
    let memo =
      Memo.create "file-tree-root" ~doc:"file tree root"
        ~input:(module Unit)
        ~output:(Simple (module Dir0))
        ~visibility:Memo.Visibility.Hidden Sync f
    in
    Memo.exec memo

  let get_vcs ~default:vcs ~path ~readdir:{ Readdir.files; dirs } =
    match
      match
        List.find_map dirs ~f:(fun (name, _, _) -> Vcs.Kind.of_filename name)
      with
      | Some kind -> Some kind
      | None -> Vcs.Kind.of_dir_contents files
    with
    | None -> vcs
    | Some kind -> Some { Vcs.kind; root = Path.(append_source root) path }

  let make_subdir =
    let module Input = struct
      type t = Dir0.t * Sub_dirs.Status.t * Readdir.t * string

      let hash ((dir : Dir0.t), _, _, s) =
        Tuple.T2.hash Path.Source.hash String.hash (dir.path, s)

      let to_dyn (dir, status, readdir, name) =
        let open Dyn.Encoder in
        record
          [ ("dir", Dir0.to_dyn dir)
          ; ("status", Sub_dirs.Status.to_dyn status)
          ; ("readdir", Readdir.to_dyn readdir)
          ; ("name", String.to_dyn name)
          ]

      let equal ((d1, _, _, n1) : t) ((d2, _, _, n2) : t) =
        Path.Source.equal d1.path d2.path && String.equal n1 n2
    end in
    let f
        ( (parent : Dir0.t)
        , (dir_status : Sub_dirs.Status.t)
        , (readdir : Readdir.t)
        , name ) =
      let path = Path.Source.relative parent.path name in
      let settings = Settings.get () in
      let project =
        if dir_status = Data_only then
          parent.project
        else
          Option.value
            (Dune_project.load ~dir:path ~files:readdir.files
               ~infer_from_opam_files:settings.recognize_jbuilder_projects)
            ~default:parent.project
      in
      let vcs = get_vcs ~default:parent.vcs ~readdir ~path in
      let contents = contents readdir ~project ~path ~dir_status in
      Dir0.create ~project ~path ~status:dir_status ~contents ~vcs
    in
    let memo =
      Memo.create "make_subdir" ~doc:"make_subdir"
        ~input:(module Input)
        ~output:(Simple (module Dir0))
        ~visibility:Memo.Visibility.Hidden Sync f
    in
    fun parent (dir_status, readdir) name ->
      Memo.exec memo (parent, dir_status, readdir, name)

  let rec find_dir t = function
    | [] -> Some t
    | comp :: components ->
      let open Option.O in
      let* subdir = String.Map.find (Dir0.sub_dirs t) comp in
      let subdir = make_subdir t subdir comp in
      find_dir subdir components

  let find_dir path =
    let t = root () in
    let components = Path.Source.explode path in
    find_dir t components
end

let root () = Memoized.root ()

let find_dir path = Memoized.find_dir path

let rec nearest_dir t = function
  | [] -> t
  | comp :: components -> (
    match String.Map.find (Dir0.sub_dirs t) comp with
    | None -> t
    | Some _ ->
      let path = Path.Source.relative (Dir0.path t) comp in
      let dir = Option.value_exn (find_dir path) in
      nearest_dir dir components )

let nearest_dir path =
  let components = Path.Source.explode path in
  nearest_dir (root ()) components

let nearest_vcs path = Dir0.vcs (nearest_dir path)

let files_of path =
  match find_dir path with
  | None -> Path.Source.Set.empty
  | Some dir ->
    Path.Source.Set.of_list
      (List.map
         (String.Set.to_list (Dir0.files dir))
         ~f:(Path.Source.relative path))

let file_exists path =
  match find_dir (Path.Source.parent_exn path) with
  | None -> false
  | Some dir -> String.Set.mem (Dir0.files dir) (Path.Source.basename path)

let dir_exists path = Option.is_some (find_dir path)

let dir_is_vendored path =
  Option.map ~f:(fun dir -> Dir0.vendored dir) (find_dir path)

module Dir = struct
  include Dir0

  let rec fold t ~traverse ~init:acc ~f =
    let must_traverse = Sub_dirs.Status.Map.find traverse t.status in
    if must_traverse then
      let acc = f t acc in
      String.Map.foldi (sub_dirs t) ~init:acc ~f:(fun dirname _ acc ->
          let dir =
            let subdir = Path.Source.relative t.path dirname in
            Option.value_exn (Memoized.find_dir subdir)
          in
          fold dir ~traverse ~init:acc ~f)
    else
      acc

  let sub_dirs (t : t) =
    t.contents.sub_dirs
    |> String.Map.mapi ~f:(fun name (_, _) ->
           let path = Path.Source.relative t.path name in
           Option.value_exn (find_dir path))
end
