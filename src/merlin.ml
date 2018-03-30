open Import
open Build.O
open! No_io

module SC = Super_context

module Preprocess = struct
  type t =
    | Pps of Jbuild.Preprocess.pps
    | Other

  let make : Jbuild.Preprocess.t -> t = function
    | Pps pps -> Pps pps
    | _       -> Other

  let merge a b =
    match a, b with
    | Other, Other -> Other
    | Pps _, Other -> a
    | Other, Pps _ -> b
    | Pps { pps = pps1; flags = flags1 },
      Pps { pps = pps2; flags = flags2 } ->
      match
        match List.compare flags1 flags2 ~compare:String.compare with
        | Eq ->
          List.compare pps1 pps2 ~compare:(fun (_, a) (_, b) ->
            Jbuild.Pp.compare a b)
        | ne -> ne
      with
      | Eq -> a
      | _  -> Other
end

type t =
  { requires   : Lib.Set.t
  ; flags      : (unit, string list) Build.t
  ; preprocess : Preprocess.t
  ; libname    : string option
  ; source_dirs: Path.Set.t
  ; objs_dirs  : Path.Set.t
  }

let make
      ?(requires=Ok [])
      ?(flags=Build.return [])
      ?(preprocess=Jbuild.Preprocess.No_preprocessing)
      ?libname
      ?(source_dirs=Path.Set.empty)
      ?(objs_dirs=Path.Set.empty)
      () =
  (* Merlin shouldn't cause the build to fail, so we just ignore errors *)
  let requires =
    match requires with
    | Ok    l -> Lib.Set.of_list l
    | Error _ -> Lib.Set.empty
  in
  { requires
  ; flags      = Build.catch flags    ~on_error:(fun _ -> [])
  ; preprocess = Preprocess.make preprocess
  ; libname
  ; source_dirs
  ; objs_dirs
  }

let add_source_dir t dir =
  { t with source_dirs = Path.Set.add t.source_dirs dir }

let ppx_flags sctx ~dir:_ ~scope ~src_dir:_ { preprocess; libname; _ } =
  match preprocess with
  | Pps { pps; flags } ->
    let exe = Preprocessing.get_ppx_driver sctx ~scope pps in
    let command =
      List.map (Path.to_absolute_filename exe
                :: "--as-ppx"
                :: Preprocessing.cookie_library_name libname
                @ flags)
        ~f:quote_for_shell
      |> String.concat ~sep:" "
    in
    [sprintf "FLG -ppx %s" (Filename.quote command)]
  | Other -> []

let dot_merlin sctx ~dir ~scope ({ requires; flags; _ } as t) =
  match Path.drop_build_context dir with
  | Some remaindir ->
    let merlin_file = Path.relative dir ".merlin" in
    (* We make the compilation of .ml/.mli files depend on the
       existence of .merlin so that they are always generated, however
       the command themselves don't read the merlin file, so we don't
       want to declare a dependency on the contents of the .merlin
       file.

       Currently jbuilder doesn't support declaring a dependency only
       on the existence of a file, so we have to use this trick. *)
    SC.add_rule sctx
      (Build.path merlin_file
       >>>
       Build.create_file (Path.relative dir ".merlin-exists"));
    SC.add_rule sctx ~mode:Promote_but_delete_on_clean (
      flags
      >>^ (fun flags ->
        let ppx_flags = ppx_flags sctx ~dir ~scope ~src_dir:remaindir t in
        let libs =
          Lib.Set.fold requires ~init:[] ~f:(fun (lib : Lib.t) acc ->
            let serialize_path = Path.reach ~from:remaindir in
            let bpath = serialize_path (Lib.obj_dir lib) in
            let spath =
              Lib.src_dir lib
              |> Path.drop_optional_build_context
              |> serialize_path
            in
            ("B " ^ bpath) :: ("S " ^ spath) :: acc
          )
        in
        let source_dirs =
          Path.Set.fold t.source_dirs ~init:[] ~f:(fun path acc ->
            let path = Path.reach path ~from:remaindir in
            ("S " ^ path)::acc
          )
        in
        let objs_dirs =
          Path.Set.fold t.objs_dirs ~init:[] ~f:(fun path acc ->
            let path = Path.reach path ~from:remaindir in
            ("B " ^ path)::acc
          )
        in
        let flags =
          match flags with
          | [] -> []
          | _  ->
            let escaped_flags = List.map ~f:quote_for_shell flags in
            ["FLG " ^ String.concat escaped_flags ~sep:" "]
        in
        let dot_merlin =
          List.concat
            [ source_dirs
            ; objs_dirs
            ; libs
            ; flags
            ; ppx_flags
            ]
        in
        dot_merlin
        |> String_set.of_list
        |> String_set.to_list
        |> List.map ~f:(Printf.sprintf "%s\n")
        |> String.concat ~sep:"")
      >>>
      Build.write_file_dyn merlin_file
    )
  | _ ->
    ()

let merge_two a b =
  { requires = Lib.Set.union a.requires b.requires
  ; flags = a.flags &&& b.flags >>^ (fun (a, b) -> a @ b)
  ; preprocess = Preprocess.merge a.preprocess b.preprocess
  ; libname =
      (match a.libname with
       | Some _ as x -> x
       | None -> b.libname)
  ; source_dirs = Path.Set.union a.source_dirs b.source_dirs
  ; objs_dirs = Path.Set.union a.objs_dirs b.objs_dirs
  }

let merge_all = function
  | [] -> None
  | init::ts -> Some (List.fold_left ~init ~f:merge_two ts)

let add_rules sctx ~dir ~scope merlin =
  if (SC.context sctx).merlin then
    dot_merlin sctx ~dir ~scope merlin
