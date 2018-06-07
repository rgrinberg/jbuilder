module T = struct
  type t = int
  let compare (a : int) b : Ordering.t =
    if a < b then
      Lt
    else if a = b then
      Eq
    else
      Gt
  let hash (x : int) = Hashtbl.hash x
  let equal (x : int) (y : int) = x = y
end

include T

module Set = Set.Make(T)
module Map = Map.Make(T)
module Table = Hashtbl.Make(T)
