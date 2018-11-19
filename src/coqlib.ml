(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018                    *)
(*     Written by: Emilio Jesús Gallego Arias  *)

open! Stdune
open Build.O
module SC = Super_context

let debug = false

(* TODO:                                       *)
(* - handle (include_subdirs ...)              *)
(* - save library information                  *)

type coq_context =
  { coqdep : Action.program
  ; coqc   : Action.program
  ; coqpp  : Action.program
  }

module CoqModule : sig
  type t
  val make : file:Path.t -> t
  (* file = .v source file; module name has to be the same so far *)
  val source : t -> Path.t
  val name : t -> string
  val obj_file : obj_dir:Path.t -> ext:string -> t -> Path.t
  val pp : t Fmt.t
end = struct

  (* A Coq module is in general of the form Foo.Bar.Mod , where
     Foo.Bar is a prefix that tends to corresponds to a file
     Foo/Bar/Mod.v *)
  type t =
    { name : string
    ; prefix : string list
    ; file : Path.t
    }

  let make ~file =
    let base_file, _ext = Path.split_extension file in
    (* Check ext = .v *)
    let name = Path.basename base_file in
    { name
    ; prefix = []
    ; file
    }

  let source x = x.file
  let name x = x.name
  let obj_file ~obj_dir ~ext x = Path.relative obj_dir (x.name ^ ext)
  let pp fmt x =
    Format.fprintf fmt "{ name = %s; source = %a }" x.name Path.pp x.file

end

let build_coq_modules ~dir ~dir_contents =
  let files = Dir_contents.text_files dir_contents in
  let v_files = String.Set.filter files ~f:(fun f -> Filename.check_suffix f ".v") in
  List.map ~f:(fun file -> CoqModule.make ~file:Path.(relative dir file)) String.Set.(to_list v_files)

let parse_coqdep ~coq_module (lines : string list) =
  let source = CoqModule.source coq_module in
  let invalid p = Errors.die "ocamldep returned invalid output for %s / [phase: %s]" Path.(to_string source) p in
  let line =
    match lines with
    | [] | _ :: _ :: _ :: _ -> invalid "line"
    | [line] -> line
    | [l1;_l2] ->
      (* .vo is produced before .vio, this is fragile tho *)
      l1
  in
  match String.lsplit2 line ~on:':' with
  | None -> invalid "split"
  | Some (basename,deps) ->
    let ff = List.hd @@ String.extract_blank_separated_words basename in
    let basename, _ext = Filename.(split_extension (basename ff)) in
    if basename <> CoqModule.name coq_module then invalid "basename";
    let deps =
      String.extract_blank_separated_words deps in
    (* Format.eprintf "deps for %a: %a@\n%!" Path.pp file pp_ls deps; *)
    deps

let gen_rule ~expander ~dir ~cc ~source_rule ~libname ~cflags coq_module =

  (* Format.eprintf "gen_rule coq_module: %s@\n%!" coq_module; *)
  let obj_dir = dir in
  let source    = CoqModule.source coq_module in
  let stdout_to = CoqModule.obj_file ~obj_dir ~ext:".v.d" coq_module in
  let object_to = CoqModule.obj_file ~obj_dir ~ext:".vo"  coq_module in

  let iflags = Arg_spec.As ["-R"; "."; libname] in
  let cd_arg = Arg_spec.[ iflags; Dep source ] in

  (*  *)
  let cd_rule =
    source_rule >>>
    Build.(run ~dir ~stdout_to cc.coqdep cd_arg)
  in

  (* Process coqdep and generate rules *)
  let deps_of = Build.dyn_paths (
    Build.lines_of stdout_to >>^
    parse_coqdep ~coq_module >>^
    List.map ~f:Path.(relative dir)
  ) in
  let cc_arg = Arg_spec.[
    iflags;
    Dep source;
    Hidden_targets [object_to] ]
  in
  [cd_rule;
   deps_of >>>
   Expander.expand_and_eval_set expander cflags ~standard:Build.(return []) >>>
   Build.run ~dir cc.coqc (Dyn (fun flags -> As flags) :: cc_arg)
  ]

let create_ccoq sctx ~dir =
  let rr prg = SC.resolve_program ~dir sctx prg ~loc:None ~hint:"try: opam install coq" in
  { coqdep = rr "coqdep"
  ; coqc   = rr "coqc"
  ; coqpp  = rr "coqpp"
  }

let gen_rules ~sctx ~dir ~dir_contents ~scope s =

  let dirs = Dir_contents.dirs dir_contents in
  let files = Dir_contents.text_files dir_contents in

  if debug then
    Format.eprintf "[gen_rules] @[dir: %a@\ndirs: @[%a@]@\ncontents: @[%a@]@\nscope: %a@]@\n%!"
      Path.pp dir
      Fmt.(list text) List.(map ~f:(fun d -> Path.to_string_maybe_quoted (Dir_contents.dir d)) dirs)
      Fmt.(list text) String.Set.(to_list files)
      Path.pp Scope.(root scope)
  ;

  let cc = create_ccoq sctx ~dir in
  let coq_modules = build_coq_modules ~dir ~dir_contents in

  (* coqdep requires all the files to be in the tree to produce correct dependencies *)
  let source_rule = Build.paths List.(map ~f:CoqModule.source coq_modules) in
  let libname = snd s.Dune_file.Coq.name in
  let cflags = s.Dune_file.Coq.flags in
  let expander = SC.expander sctx ~dir in
  let coq_rules = List.concat_map ~f:(gen_rule ~expander ~dir ~cc ~source_rule ~libname ~cflags) coq_modules in
  coq_rules

let install_rules ~sctx ~dir s =
  match s with
  | { Dune_file.Coq. public = None; _ } ->
    []
  | { Dune_file.Coq. public = Some { package = _ ; _ } ; name; _ } ->
    (* Format.eprintf "[coq] gen_install called@\n%!"; *)
    let dir_contents = Dir_contents.get sctx ~dir in
    build_coq_modules ~dir ~dir_contents
    |> List.map ~f:(fun (vfile : CoqModule.t) ->
      (* XXX: This won't work on recursive Coq libraries modules *)
      let vofile = CoqModule.obj_file ~obj_dir:dir ~ext:".vo" vfile in
      let dst = sprintf "coq/%s/%s" (snd name) Path.(basename vofile) in
      Install.(Entry.make Section.Lib_root ~dst vofile))
