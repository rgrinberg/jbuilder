open Stdune

module Alias = struct
  type t =
    { name : string
    ; dir : Path.t
    ; rec_ : bool
    }

  let to_dyn { name ; dir ; rec_ } =
    let open Dyn in
    Record
      [ "name", String name
      ; "dir", Path.to_dyn dir
      ; "rec_", Bool rec_
      ]

  let pp fmt t = Dyn.pp fmt (to_dyn t)

  let encode {name ; dir; rec_ } =
    let open Dune_lang.Encoder in
    record
      [ "name", string name
      ; "dir", Path_dune_lang.encode dir
      ; "rec_", bool rec_
      ]

  let compare x y =
    match String.compare x.name y.name with
    | Gt | Lt as x -> x
    | Eq ->
      match Path.compare x.dir y.dir with
      | Gt | Lt as x -> x
      | Eq -> Bool.compare x.rec_ y.rec_
end

module T = struct
  type t =
    | Env of Env.Var.t
    | File of Path.t
    | Universe
    | Alias of Alias.t

  let env e = Env e
  let file f = File f
  let universe = Universe

  let compare x y =
    match x, y with
    | Env x, Env y -> Env.Var.compare x y
    | Env _, (File _ | Universe | Alias _) -> Ordering.Lt
    | File x, File y -> Path.compare x y
    | File _, Env _ -> Ordering.Gt
    | File _, (Alias _ | Universe) -> Lt
    | Universe, Universe -> Ordering.Eq
    | Universe, Alias _ -> Lt
    | Universe, (Env _ | File _) -> Gt
    | Alias x, Alias y -> Alias.compare x y
    | Alias _, (Env _ | File _ | Universe) -> Gt

  let unset = lazy (Digest.string "unset")

  let trace_path p = (Path.to_string p, Utils.Cached_digest.file p)

  let trace t ~env ~stamps =
    match t with
    | Universe -> ["universe", Digest.string "universe"]
    | File p -> [trace_path p]
    | Env var ->
      let value =
        begin match Env.get env var with
        | None -> Lazy.force unset
        | Some v -> Digest.string v
        end
      in
      [var, value]
    | Alias a ->
      stamps a
      |> Path.Set.to_list
      |> List.map ~f:trace_path

  let pp fmt = function
    | Env e -> Format.fprintf fmt "Env %S" e
    | File f -> Format.fprintf fmt "File %a" Path.pp f
    | Universe -> Format.fprintf fmt "Universe"
    | Alias a -> Format.fprintf fmt "Alias %a" Alias.pp a

  let encode t =
    let open Dune_lang.Encoder in
    match t with
    | Env e -> pair string string ("Env", e)
    | File f -> pair string Path_dune_lang.encode ("File", f)
    | Universe -> string "Universe"
    | Alias a -> pair string Alias.encode ("Alias", a)
end

include T

module Set = struct
  include Set.Make(T)

  let has_universe t = mem t Universe

  let trace t ~env = List.map (to_list t) ~f:(trace ~env)

  let pp fmt (t : t) =
    Format.fprintf fmt "Deps %a" (Fmt.list pp) (to_list t)

  let add_paths t paths =
    Path.Set.fold paths ~init:t ~f:(fun p set -> add set (File p))

  let encode t = Dune_lang.Encoder.list encode (to_list t)

  let file_list t ~stamps =
    to_list t
    |> List.concat_map ~f:(function
      | Alias a -> Path.Set.to_list (stamps a)
      | File f -> [f]
      | Universe
      | Env _ -> [])

  let paths t ~stamps = Path.Set.of_list (file_list t ~stamps)

  let parallel_iter t ~f ~stamps = Fiber.parallel_iter ~f (file_list t ~stamps)

  let dirs t =
    fold t ~init:Path.Set.empty ~f:(fun f acc ->
      match f with
      | File f -> Path.Set.add acc (Path.parent_exn f)
      | Alias { Alias. dir; _ } -> Path.Set.add acc dir
      | Universe
      | Env _ -> acc)
end
