open! Stdune

module Op = struct
  type t =
    | Eq
    | Gt
    | Gte
    | Lte
    | Lt
    | Neq

  let eval t (x : Ordering.t) =
    match t, x with
    | (Eq  | Gte | Lte) , Eq
    | (Neq | Lt  | Lte) , Lt
    | (Neq | Gt  | Gte) , Gt -> true
    | _, _ -> false

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Eq -> string "Eq"
    | Gt -> string "Gt"
    | Gte -> string "Gte"
    | Lte -> string "Lte"
    | Lt -> string "Lt"
    | Neq -> string "Neq"
end

type t =
  | Const of bool
  | Expr of String_with_vars.t
  | And of t list
  | Or of t list
  | Compare of Op.t * String_with_vars.t * String_with_vars.t

let true_ = Const true

let rec eval t ~dir ~f =
  match t with
  | Const x -> x
  | Expr sw ->
    begin match String_with_vars.expand sw ~mode:Single ~dir ~f with
    | String "true" -> true
    | String "false" -> false
    | _ ->
      let loc = String_with_vars.loc sw in
      Errors.fail loc "This value must be either true or false"
    end
  | And xs -> List.for_all ~f:(eval ~f ~dir) xs
  | Or xs -> List.exists ~f:(eval ~f ~dir) xs
  | Compare (op, x, y) ->
    let x = String_with_vars.expand x ~mode:Many ~dir ~f
    and y = String_with_vars.expand y ~mode:Many ~dir ~f in
    Op.eval op (Value.L.compare_vals ~dir x y)

include (
  Dyn.Make(struct
    type nonrec t = t

    let rec to_dyn =
      let open Dyn.Encoder in
      function
      | Const b -> constr "Const" [bool b]
      | Expr e -> constr "Expr" [via_sexp String_with_vars.to_sexp e]
      | And t -> constr "And" (List.map ~f:to_dyn t)
      | Or t -> constr "Or" (List.map ~f:to_dyn t)
      | Compare (o, s1, s2) ->
        constr "Compare"
          [ Op.to_dyn o
          ; via_sexp String_with_vars.to_sexp s1
          ; via_sexp String_with_vars.to_sexp s2
          ]
  end)
)
