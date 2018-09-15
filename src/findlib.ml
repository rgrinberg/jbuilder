module Opam_package = Package
include Dfindlib.Findlib
open Stdune

let dummy_package t ~name =
  let dir =
    match path t with
    | [] -> stdlib_dir t
    | dir :: _ ->
      Lib_name.package_name name
      |> Opam_package.Name.to_string
      |> Path.relative dir
  in
  Package.create
    ~meta_file:(Path.relative dir "META")
    ~name ~dir ~vars:Dfindlib.Findlib.Vars.empty
