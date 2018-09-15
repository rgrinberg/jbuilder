type t = exn

exception Code_error of Sexp.t

exception Fatal_error of string

exception Loc_error of Loc.t * string

external raise         : exn -> _ = "%raise"
external raise_notrace : exn -> _ = "%raise_notrace"
external reraise       : exn -> _ = "%reraise"

let protectx x ~f ~finally =
  match f x with
  | y           -> finally x; y
  | exception e -> finally x; raise e

let protect ~f ~finally = protectx () ~f ~finally

let err_buf = Buffer.create 128
let err_ppf = Format.formatter_of_buffer err_buf
let kerrf fmt ~f =
  Format.kfprintf
    (fun ppf ->
       Format.pp_print_flush ppf ();
       let s = Buffer.contents err_buf in
       Buffer.clear err_buf;
       f s)
    err_ppf fmt

let fail t fmt =
  Format.pp_print_as err_ppf 7 ""; (* "Error: " *)
  kerrf fmt ~f:(fun s ->
    raise (Loc_error (t, s)))

let fail_lex lb fmt =
  fail (Loc.of_lexbuf lb) fmt

let fatalf ?loc fmt =
  kerrf ~f:(fun s ->
    match loc with
    | None -> raise (Fatal_error s)
    | Some loc -> raise (Loc_error (loc, s))
  ) fmt

let code_error message vars =
  Code_error
    (List (Atom message
           :: List.map vars ~f:(fun (name, value) ->
             Sexp.List [Atom name; value])))
  |> raise

include
  ((struct
    [@@@warning "-32-3"]
    let raise_with_backtrace exn _ = reraise exn
    include Printexc
    let raise_with_backtrace exn bt = raise_with_backtrace exn bt
  end) : (sig
     val raise_with_backtrace: exn -> Printexc.raw_backtrace -> _
   end))
