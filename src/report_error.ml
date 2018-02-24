open Import

let map_fname = ref (fun x -> x)

type printer =
  { loc       : Loc.t option
  ; pp        : Format.formatter -> unit
  ; hint      : string option
  ; backtrace : bool
  }

let p =
  { loc       = None
  ; pp        = ignore
  ; hint      = None
  ; backtrace = false
  }

let reporters = ref []
let register f = reporters := f :: !reporters

(* Firt return value is [true] if the backtrace was printed *)
let report_with_backtrace exn =
  match List.find_map !reporters ~f:(fun f -> f exn) with
  | Some p -> p
  | None ->
    match exn with
    | Loc.Error (loc, msg) ->
      let loc =
        { loc with
          start = { loc.start with pos_fname = !map_fname loc.start.pos_fname }
        }
      in
      let pp ppf = Format.fprintf ppf "@{<error>Error@}: %s\n" msg in
      { p with loc = Some loc; pp }
    | Usexp.Parser.Error e ->
      let pos = Usexp.Parser.Error.position e in
      let msg = Usexp.Parser.Error.message e in
      let pos = { pos with pos_fname = !map_fname pos.pos_fname } in
      let loc = { Loc. start = pos; stop = pos } in
      { p with
        loc = Some loc
      ; pp  = fun ppf -> Format.fprintf ppf "@{<error>Error@}: %s\n" msg
      }
    | Fatal_error msg ->
      { p with pp = fun ppf ->
          if msg.[String.length msg - 1] = '\n' then
            Format.fprintf ppf "%s" msg
          else
            Format.fprintf ppf "%s\n" (String.capitalize_ascii msg)
      }
    | Code_error msg ->
      { p with
        backtrace = true
      ; pp = fun ppf ->
          Format.fprintf ppf "@{<error>Internal error, please report upstream \
                              including the contents of _build/log.@}\n\
                              Description: %s\n"
            msg
      }
    | Unix.Unix_error (err, func, fname) ->
      { p with pp = fun ppf ->
          Format.fprintf ppf "@{<error>Error@}: %s: %s: %s\n"
            func fname (Unix.error_message err)
      }
    | _ ->
      { p with
        backtrace = true
      ; pp = fun ppf ->
          let s = Printexc.to_string exn in
          if String.is_prefix s ~prefix:"File \"" then
            Format.fprintf ppf "%s\n" s
          else
            Format.fprintf ppf "@{<error>Error@}: exception %s\n" s
      }

let report_aux ppf ?(dependency_path=[]) exn =
  let backtrace = Printexc.get_raw_backtrace () in
  let p = report_with_backtrace exn in
  let loc = if p.loc = Some Loc.none then None else p.loc in
  Option.iter loc ~f:(fun loc -> Loc.print ppf loc);
  p.pp ppf;
  if p.backtrace || !Clflags.debug_backtraces then
    Format.fprintf ppf "Backtrace:\n%s"
      (Printexc.raw_backtrace_to_string backtrace);
  let dependency_path =
    if !Clflags.debug_dep_path then
      dependency_path
    else
      (* Only keep the part that doesn't come from the build system *)
      let rec drop : With_required_by.Entries.t -> _ = function
        | (Path _ | Alias _) :: l -> drop l
        | l -> l
      in
      match loc with
      | None -> drop dependency_path
      | Some loc ->
        if Filename.is_relative loc.start.pos_fname then
          (* If the error points to a local file, no need to print the
             dependency stack *)
          []
        else
          drop dependency_path
  in
  if dependency_path <> [] then
    Format.fprintf ppf "%a@\n" With_required_by.Entries.pp
      (List.rev dependency_path);
  Option.iter p.hint ~f:(fun s -> Format.fprintf ppf "Hint: try: %s\n" s);
  Format.pp_print_flush ppf ()

let reported = ref String_set.empty

let report exn =
  let exn, dependency_path = With_required_by.unwrap_exn exn in
  match exn with
  | Already_reported -> ()
  | _ ->
    report_aux err_ppf ?dependency_path exn;
    Format.pp_print_flush err_ppf ();
    let s = Buffer.contents err_buf in
    Buffer.clear err_buf;
    (* To avoid keeping huge errors in memory *)
    let hash = Digest.string s in
    if not (String_set.mem hash !reported) then begin
      reported := String_set.add hash !reported;
      print_to_console s
    end
