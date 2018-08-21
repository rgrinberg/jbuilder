type t =
  | Atom of string
  | List of t list

module Of_sexp = struct
  type sexp = t
  type 'a t = sexp -> 'a
end

module To_sexp = struct
  type sexp = t
  type 'a t = 'a -> sexp
end
