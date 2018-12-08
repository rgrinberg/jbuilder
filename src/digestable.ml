open Stdune

module For_digest = struct
  type t = E : _ -> t
end

open For_digest

type 'a t = 'a -> For_digest.t

let raw t = E t

let list f xs = E (List.map ~f xs)

let string s = E s

let string_opt s = E s

let opt f = function
  | None -> E None
  | Some x -> E (f x)

let int s = E s

let bool s = E s

let string_set s = E (String.Set.to_list s)

let float t = E t

let path p = E (Path.to_string p)

let paths ps = E (List.map ~f:Path.to_string ps)

let env_vars (env, vars) =
  E (
    vars
    |> Env.Var.Set.to_list
    |> List.map ~f:(fun var ->
      match Env.get env var with
      | None -> "unset"
      | Some v -> v)
  )

let trace_paths paths =
  E (
    List.map paths ~f:(fun fn ->
      (Path.to_string fn, Utils.Cached_digest.file fn)))

let trace_paths_set paths =
  E (trace_paths (Path.Set.to_list paths))

let t2 fa fb (a, b) =
  let (E a) = fa a in
  let (E b) = fb b in
  E (a, b)

let t3 fa fb fc (a, b, c) =
  let (E a) = fa a in
  let (E b) = fb b in
  let (E c) = fc c in
  E (a, b, c)

let t4 fa fb fc fd (a, b, c, d) =
  let (E a) = fa a in
  let (E b) = fb b in
  let (E c) = fc c in
  let (E d) = fd d in
  E (a, b, c, d)

let to_string f t =
  Marshal.to_string (f t) []

let digest f t = Digest.string (to_string f t)
