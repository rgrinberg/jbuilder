open Import

let system_shell_exn =
  let cmd, arg, os =
    if Sys.win32 then
      ("cmd", "/c", "on Windows")
    else
      ("sh", "-c", "")
  in
  let bin = lazy (Bin.which cmd) in
  fun ~needed_to ->
    match Lazy.force bin with
    | Some path -> (path, arg)
    | None ->
      die "I need %s to %s but I couldn't find it :(\n\
           Who doesn't have %s%s?!"
        cmd needed_to cmd os

let bash_exn =
  let bin = lazy (Bin.which "bash") in
  fun ~needed_to ->
    match Lazy.force bin with
    | Some path -> path
    | None ->
      die "I need bash to %s but I couldn't find it :("
        needed_to

let signal_name =
  let table =
    let open Sys in
    [ sigabrt   , "ABRT"
    ; sigalrm   , "ALRM"
    ; sigfpe    , "FPE"
    ; sighup    , "HUP"
    ; sigill    , "ILL"
    ; sigint    , "INT"
    ; sigkill   , "KILL"
    ; sigpipe   , "PIPE"
    ; sigquit   , "QUIT"
    ; sigsegv   , "SEGV"
    ; sigterm   , "TERM"
    ; sigusr1   , "USR1"
    ; sigusr2   , "USR2"
    ; sigchld   , "CHLD"
    ; sigcont   , "CONT"
    ; sigstop   , "STOP"
    ; sigtstp   , "TSTP"
    ; sigttin   , "TTIN"
    ; sigttou   , "TTOU"
    ; sigvtalrm , "VTALRM"
    ; sigprof   , "PROF"
    (* These ones are only available in OCaml >= 4.03 *)
    ; -22       , "BUS"
    ; -23       , "POLL"
    ; -24       , "SYS"
    ; -25       , "TRAP"
    ; -26       , "URG"
    ; -27       , "XCPU"
    ; -28       , "XFSZ"
    ]
  in
  fun n ->
    match List.assoc n table with
    | exception Not_found -> sprintf "%d\n" n
    | s -> s

let jbuild_name_in ~dir =
  match Path.extract_build_context dir with
  | None ->
    Path.to_string_maybe_quoted (Path.relative dir "jbuild")
  | Some (ctx_name, dir) ->
    sprintf "%s (context %s)"
      (Path.to_string_maybe_quoted (Path.relative dir "jbuild"))
      ctx_name

let describe_target fn =
  match Path.extract_build_context fn with
  | Some (".aliases", fn) ->
    let name =
      let fn = Path.to_string fn in
      match String.rsplit2 fn ~on:'-' with
      | None -> assert false
      | Some (name, digest) ->
        assert (String.length digest = 32);
        name
    in
    sprintf "alias %s" (maybe_quoted name)
  | _ ->
    Path.to_string_maybe_quoted fn

let program_not_found ?context ?hint prog =
  die "@{<error>Error@}: Program %s not found in the tree or in PATH%s%a"
    (maybe_quoted prog)
    (match context with
     | None -> ""
     | Some name -> sprintf " (context: %s)" name)
    (fun fmt -> function
       | None -> ()
       | Some h -> Format.fprintf fmt "@ Hint: %s" h)
    hint

let library_not_found ?context ?hint lib =
  die "@{<error>Error@}: Library %s not found%s%a" (maybe_quoted lib)
    (match context with
     | None -> ""
     | Some name -> sprintf " (context: %s)" name)
    (fun fmt -> function
       | None -> ()
       | Some h -> Format.fprintf fmt "@ Hint: %s" h)
    hint

let g () =
  if !Clflags.g then
    ["-g"]
  else
    []

let find_module ~dir modules name =
  String_map.find_exn name modules
    ~string_of_key:(sprintf "%S")
    ~desc:(fun _ ->
      sprintf "<module name to module info in %s>"
        (Path.to_string_maybe_quoted dir))

let find_deps ~dir dep_graph name =
  String_map.find_exn name dep_graph
    ~string_of_key:(sprintf "%S")
    ~desc:(fun _ -> sprintf "<dependency graph in %s>"
                      (Path.to_string_maybe_quoted dir))

let obj_name_of_basename fn =
  match String.index fn '.' with
  | None -> fn
  | Some i -> String.sub fn ~pos:0 ~len:i

let install_file ~package ~findlib_toolchain =
  match findlib_toolchain with
  | None -> package ^ ".install"
  | Some x -> sprintf "%s-%s.install" package x

module Cached_digest = struct
  type file =
    { mutable digest            : Digest.t
    ; mutable timestamp         : float
    ; mutable timestamp_checked : bool
    }

  let cache = Hashtbl.create 1024

  let file fn =
    match Hashtbl.find cache fn with
    | Some x ->
      if x.timestamp_checked then
        x.digest
      else begin
        let mtime = (Unix.stat (Path.to_string fn)).st_mtime in
        if mtime <> x.timestamp then begin
          let digest = Digest.file (Path.to_string fn) in
          x.digest    <- digest;
          x.timestamp <- mtime;
        end;
        x.timestamp_checked <- true;
        x.digest
      end
    | None ->
      let digest = Digest.file (Path.to_string fn) in
      Hashtbl.add cache ~key:fn
        ~data:{ digest
              ; timestamp = (Unix.stat (Path.to_string fn)).st_mtime
              ; timestamp_checked = true
              };
      digest

  let remove fn =
    match Hashtbl.find cache fn with
    | None -> ()
    | Some file -> file.timestamp_checked <- false

  let db_file = "_build/.digest-db"

  let dump () =
    let module Pmap = Path.Map in
    let sexp =
      Sexp.List (
        Hashtbl.fold cache ~init:Pmap.empty ~f:(fun ~key ~data acc ->
          Pmap.add acc ~key ~data)
        |> Path.Map.bindings
        |> List.map ~f:(fun (path, file) ->
          Sexp.List [ Atom (Path.to_string path)
                    ; Atom (Digest.to_hex file.digest)
                    ; Atom (Int64.to_string (Int64.bits_of_float file.timestamp))
                    ]))
    in
    if Sys.file_exists "_build" then
      Io.write_file db_file (Sexp.to_string sexp)

  let load () =
    if Sys.file_exists db_file then begin
      let sexp = Sexp_lexer.Load.single db_file in
      let bindings =
        let open Sexp.Of_sexp in
        list
          (triple
             Path.t
             (fun s -> Digest.from_hex (string s))
             (fun s -> Int64.float_of_bits (Int64.of_string (string s)))
          ) sexp
      in
      List.iter bindings ~f:(fun (path, digest, timestamp) ->
        Hashtbl.add cache ~key:path
          ~data:{ digest
                ; timestamp
                ; timestamp_checked = false
                });
    end
end
