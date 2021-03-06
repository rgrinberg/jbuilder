open Stdune

type 'data t =
  { src_dir         : Path.Source.t
  ; ctx_dir         : Path.t
  ; data            : 'data
  ; scope           : Scope.t
  ; kind            : Dune_lang.File_syntax.t
  ; dune_version    : Syntax.Version.t
  }

let data t = t.data

let map t ~f = { t with data = f t.data }

let rec deep_fold l ~init ~f =
  match l with
  | [] -> init
  | t :: l -> inner_fold t t.data l ~init ~f

and inner_fold t inner_list l ~init ~f =
  match inner_list with
  | [] -> deep_fold l ~init ~f
  | x :: inner_list ->
    inner_fold t inner_list l ~init:(f t x init) ~f
