(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(*     Written by: Emilio Jesús Gallego Arias  *)

open! Stdune

type t =
  { name : Lib_name.t
  ; wrapper : string
  ; src_root : Path.t
  ; obj_root : Path.t
  ; package : Package.Name.t option
  }

let name l = l.name
let wrapper l = l.wrapper
let src_root l = l.src_root
let obj_root l = l.obj_root
let package l = l.package

module DB = struct

  type nonrec t = t Lib_name.Map.t

  let create_from_stanza (dir, s : Path.t * Dune_file.Coq.t) =
    let name = Dune_file.Coq.best_name s in
    name,
    { name
    ; wrapper = Lib_name.Local.to_string (snd s.name)
    ; obj_root = dir
    ; src_root = dir
    ; package = Option.map s.public ~f:(fun p -> p.package.name)
    }

  (* XXX: Error handling: register errors and printers *)
  let create_from_coqlib_stanzas sl =
    match Lib_name.Map.of_list_map ~f:create_from_stanza sl with
    | Ok m -> m
    | Error (name, _w1, w2) ->
      let loc = (snd w2).loc in
      raise (Errors.exnf loc "Duplicate theory name: %a" Lib_name.pp name)

  let resolve db (loc,name) =
    match Lib_name.Map.find db name with
    | None ->
      Error (Errors.exnf loc "Theory %a not found" Lib_name.pp name)
    | Some s -> Ok s

  let find_many t ~loc =
    Result.List.map ~f:(fun name -> resolve t (loc, name))

end
