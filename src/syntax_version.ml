open Import

type t = int * int

let sexp_of_t (a, b) = Sexp.unsafe_atom_of_string (sprintf "%u.%u" a b)

let t_of_sexp : t Sexp.Of_sexp.t = function
  | Atom (loc, A s) -> begin
      try
        Scanf.sscanf s "%u.%u" (fun a b -> (a, b))
      with _ ->
        Loc.fail loc "atom of the form NNN.NNN expected"
    end
  | sexp ->
    Sexp.Of_sexp.of_sexp_error sexp "atom expected"

let can_read ~parser_version:(pa, pb) ~data_version:(da, db) =
  pa = da && db <= pb
