open Stdune

exception Invalid_lib_name of string

module Local = struct
  type t = string

  type result =
    | Ok of t
    | Warn of t
    | Invalid

  let valid_char = function
    | 'A'..'Z' | 'a'..'z' | '_' | '0'..'9' -> true
    | _ -> false

  let of_string (name : string) =
    match name with
    | "" -> Invalid
    | (s : string) ->
      if s.[0] = '.' then
        Invalid
      else
        let len = String.length s in
        let rec loop warn i =
          if i = len - 1 then
            if warn then Warn s else Ok s
          else
            let c = String.unsafe_get s i in
            if valid_char c then
              loop warn (i + 1)
            else if c = '.' then
              loop true (i + 1)
            else
              Invalid
        in
        loop false 0

  let of_string_exn s =
    match of_string s with
    | Ok s -> s
    | Warn _
    | Invalid -> raise (Invalid_lib_name s)

  let dparse_loc =
    Dsexp.Of_sexp.plain_string (fun ~loc s -> (loc, of_string s))

  let dgen = Dsexp.To_sexp.string

  let to_sexp = Sexp.To_sexp.string

  let pp_quoted fmt t = Format.fprintf fmt "%S" t
  let pp fmt t = Format.fprintf fmt "%s" t

  let invalid_message =
    "invalid library name.\n\
     Hint: library names must be non-empty and composed only of \
     the following characters: 'A'..'Z',  'a'..'z', '_'  or '0'..'9'"

  let wrapped_message =
    sprintf
      "%s.\n\
       This is temporary allowed for libraries with (wrapped false).\
       \nIt will not be supported in the future. \
       Please choose a valid name field."
      invalid_message

  let validate (loc, res) ~wrapped =
    match res, wrapped with
    | Ok s, _ -> s
    | Warn _, true -> Errors.fail loc "%s" wrapped_message
    | Warn s, false -> Errors.warn loc "%s" wrapped_message; s
    | Invalid, _ -> Errors.fail loc "%s" invalid_message

  let to_string s = s
end

let pp = Dfindlib.Lib_name.pp
let pp_quoted = Dfindlib.Lib_name.pp_quoted
let compare = Dfindlib.Lib_name.compare
let to_string = Dfindlib.Lib_name.to_string
let of_string_exn = Dfindlib.Lib_name.of_string_exn
let nest  = Dfindlib.Lib_name.nest

let to_sexp t = Sexp.Atom (to_string t)

let to_local s = Local.of_string (to_string s)

let dgen s = Dsexp.To_sexp.string (to_string s)
let dparse =
  let open Dsexp.Of_sexp in
  string >>| of_string_exn ~loc:None

let of_local (loc, t) = of_string_exn ~loc:(Some loc) t

type t = Dfindlib.Lib_name.t

module Map = Dfindlib.Lib_name.Map
module Set = Dfindlib.Lib_name.Set

let root_lib = Dfindlib.Lib_name.root_lib

let split t =
  match String.split (to_string t) ~on:'.' with
  | [] -> assert false
  | pkg :: rest -> (Package.Name.of_string pkg, rest)

let package_name t =
  Package.Name.of_string (to_string (root_lib t))

module L = struct
  let to_key (xs : t list) =
    match (xs :> string list) with
    | [] -> "+none+"
    | names  -> String.concat ~sep:"+" names
end
