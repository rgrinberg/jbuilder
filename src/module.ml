open Import

module Syntax = struct
  type t = OCaml | Reason
end

module File = struct
  type t =
    { name : string
    ; syntax : Syntax.t
    }

  let to_ocaml t =
    match t.syntax with
    | OCaml -> code_errorf "to_ocaml: can only convert reason Files" ()
    | Reason ->
      { syntax = OCaml
      ; name =
          let base, ext = Filename.split_extension t.name in
          base ^ ".re" ^
          (match Filename.extension t.name with
           | ".re" -> ".ml"
           | ".rei" -> ".mli"
           | _ -> code_errorf "to_ocaml: unrecognized extension %s" ext ())
      }
end

type t =
  { name     : string
  ; impl     : File.t option
  ; intf     : File.t option
  ; obj_name : string
  }

let name t = t.name

let real_unit_name t = String.capitalize_ascii (Filename.basename t.obj_name)

let file t ~dir (kind : Ml_kind.t) =
  let file =
    match kind with
    | Impl -> t.impl
    | Intf -> t.intf
  in
  Option.map file ~f:(fun f -> Path.relative dir f.name)

let cm_source t ~dir kind = file t ~dir (Cm_kind.source kind)

let cm_file t ~dir kind = Path.relative dir (t.obj_name ^ Cm_kind.ext kind)

let cmt_file t ~dir (kind : Ml_kind.t) =
  match kind with
  | Impl -> Option.map t.impl ~f:(fun _ -> Path.relative dir (t.obj_name ^ ".cmt" ))
  | Intf -> Option.map t.intf ~f:(fun _ -> Path.relative dir (t.obj_name ^ ".cmti"))

let odoc_file t ~doc_dir = Path.relative doc_dir (t.obj_name ^ ".odoc")

let cmti_file t ~dir =
  match t.intf with
  | None   -> Path.relative dir (t.obj_name ^ ".cmt")
  | Some _ -> Path.relative dir (t.obj_name ^ ".cmti")

let iter t ~f =
  Option.iter t.impl ~f:(f Ml_kind.Impl);
  Option.iter t.intf ~f:(f Ml_kind.Intf)

let set_obj_name t ~wrapper =
  match wrapper with
  | Some s -> { t with obj_name = sprintf "%s__%s" s t.name }
  | None ->
    let fn =
      match t.impl with
      | Some f -> f.name
      | None -> (Option.value_exn t.intf).name
    in
    let obj_name  =
      match String.index fn '.' with
      | None -> fn
      | Some i -> String.sub fn ~pos:0 ~len:i
    in
    { t with obj_name }
