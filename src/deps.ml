open! Import

type t =
  { paths : Path.Set.t
  ; vars : Env.Var.Set.t
  }

let paths t = t.paths

let digest env {paths; vars} =
  Digestable.t2
    Digestable.trace_paths_set
    (Digestable.env_vars)
    (paths, (env, vars))

let union {paths = paths_a; vars = vars_a} {paths = paths_b; vars = vars_b} =
  { paths = Path.Set.union paths_a paths_b
  ; vars = Env.Var.Set.union vars_a vars_b
  }

let path_union a b =
  Path.Set.union a.paths b.paths

let path_diff a b =
  Path.Set.diff a.paths b.paths

let empty =
  { paths = Path.Set.empty
  ; vars = Env.Var.Set.empty
  }

let add_path t path =
  { t with
    paths = Path.Set.add t.paths path
  }

let add_paths t fns =
  { t with
    paths = Path.Set.union t.paths fns
  }

let add_env_var t var =
  { t with
    vars = Env.Var.Set.add t.vars var
  }

let to_sexp {paths; vars} =
  let sexp_paths =
    Dune_lang.Encoder.list Path_dune_lang.encode (Path.Set.to_list paths)
  in
  let sexp_vars =
    Dune_lang.Encoder.list Dune_lang.Encoder.string (Env.Var.Set.to_list vars)
  in
  Dune_lang.Encoder.record
    [ ("paths", sexp_paths)
    ; ("vars", sexp_vars)
    ]
