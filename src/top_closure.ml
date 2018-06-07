open Import

module type Keys = sig
  type t
  type elt
  val empty : t
  val add : t -> elt -> t
  val mem : t -> elt -> bool
end

module type S = sig
  type key
  val top_closure
    :  key:('a -> key)
    -> deps:('a -> 'a list)
    -> 'a list
    -> ('a list, 'a list) result
end

module type Keys_mutable = sig
  type t
  type elt
  val create : unit -> t
  val add : t -> elt -> unit
  val mem : t -> elt -> bool
end

module To_mutable(K : Keys) = struct
  type t = K.t ref
  type elt = K.elt
  let create () = ref K.empty
  let add t x = t := K.add !t x
  let mem t x = K.mem !t x
end

module Of_table(T : Hashtbl.S) = struct
  type t = unit T.t
  type elt = T.key
  let create () = T.create 16
  let add t x = T.replace t ~key:x ~data:()
  let mem = T.mem
end

module Make(Keys : Keys_mutable) = struct
  let top_closure ~key ~deps elements =
    let visited = Keys.create () in
    let res = ref [] in
    let rec loop elt ~temporarily_marked =
      let key = key elt in
      if List.mem ~set:temporarily_marked key then
        Error [elt]
      else if not (Keys.mem visited key) then begin
        Keys.add visited key;
        let temporarily_marked = key :: temporarily_marked in
        match iter_elts (deps elt) ~temporarily_marked with
        | Ok () -> res := elt :: !res; Ok ()
        | Error l -> Error (elt :: l)
      end else
        Ok ()
    and iter_elts elts ~temporarily_marked =
      match elts with
      | [] -> Ok ()
      | elt :: elts ->
        match loop elt ~temporarily_marked with
        | Error _ as result -> result
        | Ok () -> iter_elts elts ~temporarily_marked
    in
    match iter_elts elements ~temporarily_marked:[] with
    | Ok () -> Ok (List.rev !res)
    | Error elts -> Error elts
end

module Int    = Make(Of_table(Int.Table))
module String = Make(Of_table(String.Table))
