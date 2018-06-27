open! Import

let local_install_dir =
  let dir = Path.relative Path.build_dir "install" in
  fun ~context -> Path.relative dir context

let local_install_bin_dir ~context =
  Path.relative (local_install_dir ~context) "bin"

let local_install_man_dir ~context =
  Path.relative (local_install_dir ~context) "bin"

let local_install_lib_dir ~context ~package =
  Path.relative
    (Path.relative (local_install_dir ~context) "lib")
    package

let dev_null =
  Path.of_filename_relative_to_initial_cwd
    (if Sys.win32 then "nul" else "/dev/null")

let jbuilder_keep_fname = ".jbuilder-keep"

let inside_emacs = Option.is_some (Env.get Env.initial "INSIDE_EMACS")
let inside_dune  = Option.is_some (Env.get Env.initial "INSIDE_DUNE")

let default_build_profile =
  match Which_program.t with
  | Dune     -> "dev"
  | Jbuilder -> "release"

open Sexp.Of_sexp

module Display = struct
  type t =
    | Progress
    | Short
    | Verbose
    | Quiet

  let all =
      [ "progress" , Progress
      ; "verbose"  , Verbose
      ; "short"    , Short
      ; "quiet"    , Quiet
      ]

  let t = enum all
end

module Concurrency = struct
  type t =
    | Fixed of int
    | Auto

  let error =
    Error "invalid concurrency value, must be 'auto' or a positive number"

  let of_string = function
    | "auto" -> Ok Auto
    | s ->
      match int_of_string s with
      | exception _ -> error
      | n ->
        if n >= 1 then
          Ok (Fixed n)
        else
          error

  let t =
    plain_string (fun ~loc s ->
      match of_string s with
      | Error m -> of_sexp_errorf loc "%s" m
      | Ok s -> s)

  let to_string = function
    | Auto -> "auto"
    | Fixed n -> string_of_int n
end

module type S = sig
  type 'a field

  type t =
    { display     : Display.t     field
    ; concurrency : Concurrency.t field
    }
end

module rec M : S with type 'a field = 'a = M
include M

module rec Partial : S with type 'a field := 'a option = Partial

let merge t (partial : Partial.t) =
  let field from_t from_partial =
    Option.value from_partial ~default:from_t
  in
  { display     = field t.display     partial.display
  ; concurrency = field t.concurrency partial.concurrency
  }

let default =
  { display     = if inside_dune then Quiet   else Progress
  ; concurrency = if inside_dune then Fixed 1 else Auto
  }

let t =
  record
    (field "display" Display.t ~default:default.display
     >>= fun display ->
     field "jobs" Concurrency.t ~default:default.concurrency
     >>= fun concurrency ->
     return { display
            ; concurrency
            })

let user_config_file =
  Path.relative (Path.of_filename_relative_to_initial_cwd Xdg.config_dir)
    "dune/config"

let load_config_file p =
  (Sexp.Of_sexp.parse t Univ_map.empty) (Io.Sexp.load p ~mode:Many_as_one)

let load_user_config_file () =
  if Path.exists user_config_file then
    load_config_file user_config_file
  else
    default

let adapt_display config ~output_is_a_tty =
  if config.display = Progress &&
     not output_is_a_tty &&
     not inside_emacs
  then
    { config with display = Quiet }
  else
    config
