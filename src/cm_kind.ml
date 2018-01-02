type t = Cmi | Cmo | Cmx

let pp fmt = function
  | Cmi -> Format.pp_print_string fmt "Cmi"
  | Cmo -> Format.pp_print_string fmt "Cmo"
  | Cmx -> Format.pp_print_string fmt "Cmx"

let all = [Cmi; Cmo; Cmx]

let choose cmi cmo cmx = function
  | Cmi -> cmi
  | Cmo -> cmo
  | Cmx -> cmx

let ext = choose ".cmi" ".cmo" ".cmx"

let source = choose Ml_kind.Intf Impl Impl

module Dict = struct
  type 'a t =
    { cmi : 'a
    ; cmo : 'a
    ; cmx : 'a
    }

  let get t = function
    | Cmi -> t.cmi
    | Cmo -> t.cmo
    | Cmx -> t.cmx

  let of_func f =
    { cmi = f ~cm_kind:Cmi
    ; cmo = f ~cm_kind:Cmo
    ; cmx = f ~cm_kind:Cmx
    }
end
