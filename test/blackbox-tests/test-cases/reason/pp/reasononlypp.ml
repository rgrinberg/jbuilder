let lint = ref false
let fname = ref None
let usage =
  Printf.sprintf "%s [-lint] file" (Filename.basename Sys.executable_name)
let anon s =
  match !fname with
  | None -> fname := Some s
  | Some _ -> raise (Arg.Bad "file must only be given once")

let () =
  Arg.parse
    ["-lint", Arg.Set lint, "lint instead of preprocessing"
    ] anon usage;
  let fname =
    match !fname with
    | None -> raise (Arg.Bad "file must be provided")
    | Some f -> f in

  if Filename.check_suffix fname ".re"
  || Filename.check_suffix fname ".rei" then (
    if !lint then (
      exit 0
    );
    let ch = open_in fname in
    let rec loop () =
      match input_line ch with
      | exception End_of_file -> ()
      | line -> print_endline line; loop () in
    loop ();
    close_in ch
  ) else (
    Format.eprintf "%s is not a reason source@.%!" fname;
    exit 1
  )
