(** Value with a dependency path *)

type 'a t =
  { data : 'a
  ; required_by : entry list
  }

and entry =
  | Path of Path.t
  | Virt of string

(** Re-raise an exception and augment it's dependency path with the given entry. The
    raised exception will be wrapped. *)
val reraise : exn -> entry -> _

(** Extract a wrapped exception *)
val unwrap_exn : exn -> exn * entry list option
