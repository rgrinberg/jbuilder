open Import

include Interned.Make()

let ppx_driver = make "ppx_driver"
let mt         = make "mt"
let mt_posix   = make "mt_posix"
let byte       = make "byte"
let native     = make "native"
let plugin     = make "plugin"

(* An assignment or addition *)
module Rule = struct
  type t =
    { preds_required  : Set.t
    ; preds_forbidden : Set.t
    ; value           : string
    }

  let formal_predicates_count t =
    Set.cardinal t.preds_required + Set.cardinal t.preds_forbidden

  let matches t ~preds =
    Set.is_subset t.preds_required ~of_:preds &&
    Set.is_empty (Set.inter preds t.preds_forbidden)

  let make (rule : Meta.rule) =
    let preds_required, preds_forbidden =
      List.partition_map rule.predicates ~f:(function
        | Pos x -> Left  x
        | Neg x -> Right x)
    in
    { preds_required  = Set.make preds_required
    ; preds_forbidden = Set.make preds_forbidden
    ; value           = rule.value
    }
end

module Rules0 = struct
  (* To implement the algorithm, [set_rules] is sorted by decreasing
     number of formal predicates, then according to the order of the
     META file. [add_rules] are in the same order as in the META
     file. *)
  type t =
    { set_rules : Rule.t list
    ; add_rules : Rule.t list
    }

  let interpret t ~preds =
    let rec find_set_rule = function
      | [] -> ""
      | rule :: rules ->
        if Rule.matches rule ~preds then
          rule.value
        else
          find_set_rule rules
    in
    let v = find_set_rule t.set_rules in
    List.fold_left t.add_rules ~init:v ~f:(fun v rule ->
      if Rule.matches rule ~preds then
        v ^ " " ^ rule.value
      else
        v)

  let of_meta_rules (rules : Meta.Simplified.Rules.t) =
    let add_rules = List.map rules.add_rules ~f:Rule.make in
    let set_rules =
      List.map rules.set_rules ~f:Rule.make
      |> List.stable_sort ~compare:(fun a b ->
        Int.compare
          (Rule.formal_predicates_count b)
          (Rule.formal_predicates_count a))
    in
    { add_rules; set_rules }
end

type variant = t
module Rules = struct
  type _ t =
    | Pred : Rules0.t -> string t
    | List : (variant * 'a) list -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t

  let rec get : type a . a t -> variants:Set.t -> a list =
    fun t ~variants ->
      match t with
      | Pred p ->
        String.extract_comma_space_separated_words
          (Rules0.interpret p ~preds:variants)
      | List l ->
        List.filter_map l ~f:(fun (variant, a) ->
          if Set.mem variants variant then
            Some a
          else
            None
        )
      | Map (t, f) -> List.map ~f (get t ~variants)

  let map t ~f = Map (t, f)

  let make l = List l

  let of_meta_rules (s : Meta.Simplified.Rules.t) : string t =
    Pred (Rules0.of_meta_rules s)
end
