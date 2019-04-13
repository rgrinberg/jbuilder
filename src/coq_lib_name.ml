(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(*     Written by: Emilio Jesús Gallego Arias  *)

type t = Lib_name.t

let make x = x

let pp = Lib_name.pp

let to_sexp = Lib_name.to_sexp

module Map = Lib_name.Map
