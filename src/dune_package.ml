open! Stdune

module Lib = struct
  module Virtual = struct
    type t =
      { modules   : Module.Name.t list
      ; dep_graph : unit
      }

    let encode { modules; dep_graph } =
      let open Dune_lang.Encoder in
      record
        [ "modules", (list Module.Name.encode) modules
        ; "dep_graph", unit dep_graph
        ]

    let decode =
      let open Dune_lang.Decoder in
      record (
        let%map modules = field "modules" (list Module.Name.decode)
        in
        { dep_graph = ()
        ; modules
        }
      )
  end

  module Kind = struct
    type t =
      | Normal
      | Ppx_deriver
      | Ppx_rewriter

    let decode =
      let open Dune_lang.Decoder in
      enum
        [ "normal"       , Normal
        ; "ppx_deriver"  , Ppx_deriver
        ; "ppx_rewriter" , Ppx_rewriter
        ]

    let encode t =
      Dune_lang.Encoder.string (
        match t with
        | Normal -> "normal"
        | Ppx_deriver -> "ppx_deriver"
        | Ppx_rewriter -> "ppx_rewriter")
  end

  type 'sub_system t =
    { loc              : Loc.t
    ; name             : Lib_name.t
    ; dir              : Path.t
    ; kind             : Kind.t
    ; synopsis         : string option
    ; archives         : Path.t list Mode.Dict.t
    ; plugins          : Path.t list Mode.Dict.t
    ; foreign_objects  : Path.t list
    ; foreign_archives : Path.t list Mode.Dict.t
    ; jsoo_runtime     : Path.t list
    ; ppx_runtime_deps : (Loc.t * Lib_name.t) list
    ; pps              : (Loc.t * Lib_name.t) list
    ; sub_systems      : 'sub_system Sub_system_name.Map.t
    ; virtual_         : Virtual.t option
    ; implements       : (Loc.t * Lib_name.t) option
    ; main_module_name : Module.Name.t option
    ; requires         : (Loc.t * Lib_name.t) list
    ; version          : string option
    }

  let make ~loc ~kind ~name ~synopsis ~archives ~plugins ~foreign_objects
        ~foreign_archives ~jsoo_runtime ~main_module_name ~sub_systems
        ~requires ~pps ~ppx_runtime_deps ~virtual_ ~implements
        ~version ~(map_paths : Path.t -> Path.t) ~dir =
    let map_list = List.map ~f:map_paths in
    let map_mode = Mode.Dict.map ~f:map_list in
    { loc
    ; kind
    ; name
    ; synopsis
    ; archives = map_mode archives
    ; plugins = map_mode plugins
    ; foreign_objects = map_list foreign_objects
    ; foreign_archives = map_mode foreign_archives
    ; jsoo_runtime = map_list jsoo_runtime
    ; main_module_name
    ; sub_systems
    ; requires
    ; pps
    ; ppx_runtime_deps
    ; virtual_
    ; implements
    ; version
    ; dir
    }

  let dir t = t.dir

  let set_subsystems t sub_systems =
    { t with sub_systems }

  let dir_of_name name =
    let (_, components) = Lib_name.split name in
    Path.Local.L.relative Path.Local.root components

  let encode
        { loc = _ ; kind ; synopsis ; name ; archives ; plugins
        ; foreign_objects ; foreign_archives ; jsoo_runtime ; requires
        ; ppx_runtime_deps ; pps ; sub_systems ; virtual_
        ; implements ; main_module_name ; version = _; dir
        } =
    let open Dune_lang.Encoder in
    let no_loc f (_loc, x) = f x in
    let no_locs f xs = (list f) (List.map ~f:snd xs) in
    let path = Path_dune_lang.Local.encode ~dir in
    record_fields Dune
      [ field "name" Lib_name.encode name
      ; field "kind" Kind.encode kind
      ; field_o "synopsis" string synopsis
      ; field "archives"
          (Mode.Dict.encode (list path)) archives
      ; field "plugins" (Mode.Dict.encode (list path)) plugins
      ; field "foreign_objects" (list path) foreign_objects
      ; field "foreign_archives" (Mode.Dict.encode (list path))
          foreign_archives
      ; field "jsoo_runtime" (list path) jsoo_runtime
      ; field "requires" (no_locs Lib_name.encode) requires
      ; field "ppx_runtime_deps" (no_locs Lib_name.encode) ppx_runtime_deps
      ; field "pps" (no_locs Lib_name.encode) pps
      ; field_o "implements" (no_loc Lib_name.encode) implements
      ; field_o "main_module_name" Module.Name.encode main_module_name
      ; field_o "virtual" Virtual.encode virtual_
      ; field "sub_systems" (list sexp) (
          Sub_system_name.Map.to_list sub_systems
          |> List.map ~f:(fun (name, (ver, sexp)) ->
            Dune_lang.List
              [ Dune_lang.atom (Sub_system_name.to_string name)
              ; Syntax.Version.encode ver
              ; List sexp
              ]
          )
        )
      ]

  let decode ~base =
    let open Dune_lang.Decoder in
    let path = Path_dune_lang.Local.decode ~dir:base in
    record (
      let%map loc = loc
      and name = field "name" Lib_name.decode
      and synopsis = field_o "synopsis" string
      and kind = field "kind" Kind.decode
      and archives = field "archives" (Mode.Dict.decode (list path))
      and plugins = field "plugins" (Mode.Dict.decode (list path))
      and foreign_objects = field "foreign_objects" (list path)
      and foreign_archives =
        field "foreign_archives" (Mode.Dict.decode (list path))
      and jsoo_runtime = field "jsoo_runtime" (list path)
      and requires =
        field "requires" (list (located Lib_name.decode)) ~default:[]
      and ppx_runtime_deps =
        field "ppx_runtime_deps" (list (located Lib_name.decode)) ~default:[]
      and pps = field "pps" (list (located Lib_name.decode)) ~default:[]
      and main_module_name = field_o "main_module_name" Module.Name.decode
      and virtual_ = field_o "virtual" Virtual.decode
      and sub_systems =
        field ~default:[] "sub_systems"
          (list (triple (located string) (located Syntax.Version.decode) raw))
      and implements = field_o "implements" (located Lib_name.decode)
      in
      let sub_systems =
        sub_systems
        |> List.filter_map ~f:(fun ((loc, name), ver, raw) ->
          Option.map (Sub_system_name.get name) ~f:(fun name ->
            (name, (loc, ver, raw))))
        |> Sub_system_name.Map.of_list
        |> (function
          | Ok x -> x
          | Error (name, _, (loc, _, _)) ->
            Errors.fail loc "%S present twice" (Sub_system_name.to_string name))
        |> Sub_system_name.Map.map ~f:(fun (_, ver, data) -> (ver, data))
      in
      { kind
      ; name
      ; synopsis
      ; loc
      ; archives
      ; plugins
      ; foreign_objects
      ; foreign_archives
      ; jsoo_runtime
      ; requires
      ; ppx_runtime_deps
      ; pps
      ; implements
      ; sub_systems
      ; main_module_name
      ; virtual_
      ; version = None
      ; dir = Path.append_local base (dir_of_name name)
      }
    )

  let name t = t.name
  let version t = t.version
  let kind t = t.kind
  let loc t = t.loc
  let virtual_ t = t.virtual_
  let sub_systems t = t.sub_systems
  let synopsis t = t.synopsis
  let main_module_name t = t.main_module_name
  let ppx_runtime_deps t = t.ppx_runtime_deps
  let foreign_objects t = t.foreign_objects
  let archives t = t.archives
  let plugins t = t.plugins
  let jsoo_runtime t = t.jsoo_runtime
  let foreign_archives t = t.foreign_archives
  let pps t = t.pps
  let requires t = t.requires
  let implements t = t.implements

  let compare_name x y = Lib_name.compare x.name y.name
end

type 'sub_system t =
  { libs         : 'sub_system Lib.t list
  ; name         : Package.Name.t
  ; version      : string option
  }

let gen ~dune_version { libs ; name ; version } =
  let open Dune_lang.Encoder in
  let list s = Dune_lang.List s in
  let sexp =
    [ list [ Dune_lang.atom "lang"
           ; string (Syntax.name Stanza.syntax)
           ; Syntax.Version.encode dune_version
           ]
    ; list [ Dune_lang.atom "name"; Package.Name.encode name ]
    ] in
  let sexp =
    match version with
    | None -> sexp
    | Some version ->
      sexp @ [List [Dune_lang.atom "version"; Dune_lang.atom version]]
  in
  let libs =
    List.map libs ~f:(fun lib ->
      list [ Dune_lang.atom "library"
           ; List (Lib.encode lib)
           ])
  in
  sexp @ libs

let decode ~dir =
  let open Dune_lang.Decoder in
  fields (
    let%map name = field "name" Package.Name.decode
    and version = field_o "version" string
    and libs = multi_field "library" (Lib.decode ~base:dir)
    in
    { name
    ; version
    ; libs = List.map libs ~f:(fun (lib : _ Lib.t) -> { lib with version })
    }
  )

module Vfile = Versioned_file.Make(struct type t = unit end)

let () = Vfile.Lang.register Stanza.syntax ()

let load p = Vfile.load p ~f:(fun _ -> decode ~dir:(Path.parent_exn p))
