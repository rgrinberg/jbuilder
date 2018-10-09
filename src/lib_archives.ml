open Stdune

type t =
  { dlls : Path.t list
  ; files : Path.t list
  ; sub_dir : string option
  }

let install_entries { files; dlls ; sub_dir } =
  let dst =
    match sub_dir with
    | None -> Path.basename
    | Some dir -> fun p -> sprintf "%s/%s" dir (Path.basename p)
  in
  List.rev_append
    (List.map files ~f:(fun p -> Install.Entry.make Lib p ~dst:(dst p)))
    (List.map dlls ~f:(Install.Entry.make Stublibs))

let all { files; dlls ; sub_dir = _ } =
  Path.Set.of_list (List.rev_append dlls files)

module Library = Dune_file.Library

let make ~(ctx : Context.t) ~dir (lib : Library.t) =
  let { Mode.Dict.byte; native } =
    Dune_file.Mode_conf.Set.eval lib.modes
      ~has_native:(Option.is_some ctx.ocamlopt)
  in
  let if_ cond l = if cond then l else [] in
  let files =
    let virtual_library = Library.is_virtual lib in
    List.concat
      [ if_ (byte && not virtual_library)
          [ Library.archive ~dir lib ~ext:".cma" ]
      ; if virtual_library then (
          (lib.c_names @ lib.cxx_names)
          |> List.map ~f:(fun (_, c) -> Path.relative dir (c ^ ctx.ext_obj))
        ) else if Library.has_stubs lib then (
          [ Library.stubs_archive ~dir lib ~ext_lib:ctx.ext_lib ]
        ) else
          []
      ; if_ (native && not virtual_library)
          (let files =
             [ Library.archive ~dir lib ~ext:".cmxa"
             ; Library.archive ~dir lib ~ext:ctx.ext_lib
             ]
           in
           if Dynlink_supported.get lib.dynlink ctx.natdynlink_supported then
             files @ [ Library.archive ~dir lib ~ext:".cmxs" ]
           else
             files)
      ; List.map lib.buildable.js_of_ocaml.javascript_files ~f:(Path.relative dir)
      ; List.map lib.install_c_headers ~f:(fun fn ->
          Path.relative dir (fn ^ ".h"))
      ]
  in
  let dlls  =
    if_ (byte && Library.has_stubs lib &&
         Dynlink_supported.get lib.dynlink ctx.supports_shared_libraries)
      [Library.dll ~dir lib ~ext_dll:ctx.ext_dll]
  in
  let sub_dir =
    Option.bind lib.public ~f:(fun public -> public.sub_dir)
  in
  { files
  ; dlls
  ; sub_dir
  }
