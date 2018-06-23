open! Import

module Ast = struct
  [@@@warning "-37"]
  type expanded = Expanded
  type unexpanded = Unexpanded
  type ('a, _) t =
    | Element : 'a -> ('a, _) t
    | Special : Loc.t * string -> ('a, _) t
    | Union : ('a, 'b) t list -> ('a, 'b) t
    | Diff : ('a, 'b) t * ('a, 'b) t -> ('a, 'b) t
    | Include : String_with_vars.t -> ('a, unexpanded) t
end

type 'ast generic =
  { ast : 'ast
  ; loc : Loc.t option
  ; context: Univ_map.t
  }

let syntax context =
  match Univ_map.find context (Syntax.key Stanza.syntax) with
  | None
  | Some (0, _) -> Usexp.Jbuild
  | Some (_, _) -> Usexp.Dune

type ast_expanded = (Loc.t * string, Ast.expanded) Ast.t
type t = ast_expanded generic
let loc t = t.loc

let parse_general sexp ~f =
  let rec of_sexp : Sexp.Ast.t -> _ = function
    | Atom (loc, A "\\") -> Loc.fail loc "unexpected \\"
    | (Atom (_, A "") | Quoted_string (_, _) | Template _ ) as t ->
      Ast.Element (f t)
    | Atom (loc, A s) as t ->
      if s.[0] = ':' then
        Special (loc, String.sub s ~pos:1 ~len:(String.length s - 1))
      else
        Element (f t)
    | List (_, sexps) -> of_sexps [] sexps
  and of_sexps acc = function
    | Atom (_, A "\\") :: sexps ->
       Diff (Union (List.rev acc), of_sexps [] sexps)
    | elt :: sexps ->
      of_sexps (of_sexp elt :: acc) sexps
    | [] -> Union (List.rev acc)
  in
  of_sexp sexp

let t =
  let open Sexp.Of_sexp in
  context >>= fun context ->
  raw >>| fun sexp ->
  let ast =
    parse_general sexp ~f:(function
      | Template t -> no_templates_in t.loc "expanded ordered set fields"
      | Atom (loc, A s) | Quoted_string (loc, s) -> (loc, s)
      | List _ -> assert false)
  in
  { ast
  ; loc = Some (Sexp.Ast.loc sexp)
  ; context
  }

let is_standard t =
  match (t.ast : ast_expanded) with
  | Ast.Special (_, "standard") -> true
  | _ -> false

module type Value = sig
  type t
  type key
  val key : t -> key
end

module type Key = sig
  type t
  val compare : t -> t -> Ordering.t
  module Map : Map.S with type key = t
end

module type S = sig
  type value
  type 'a map

  val eval
    :  t
    -> parse:(loc:Loc.t -> string -> value)
    -> standard:value list
    -> value list

  val eval_unordered
    :  t
    -> parse:(loc:Loc.t -> string -> value)
    -> standard:value map
    -> value map
end

module Make(Key : Key)(Value : Value with type key = Key.t) = struct
  module type Named_values = sig
    type t

    val singleton : Value.t -> t
    val union : t list -> t
    val diff : t -> t -> t
  end

  module Make(M : Named_values) = struct
    let eval t ~parse ~special_values =
      let rec of_ast (t : ast_expanded) =
        let open Ast in
        match t with
        | Element (loc, s) ->
          let x = parse ~loc s in
          M.singleton x
        | Special (loc, name) -> begin
            match String.Map.find special_values name with
            | Some x -> x
            | None   -> Loc.fail loc "undefined symbol %s" name
          end
        | Union elts -> M.union (List.map elts ~f:of_ast)
        | Diff (left, right) ->
          let left  = of_ast left  in
          let right = of_ast right in
          M.diff left right
      in
      of_ast t.ast
  end

  module Ordered = Make(struct
      type t = Value.t list

      let singleton x = [x]
      let union = List.flatten
      let diff a b =
        List.filter a ~f:(fun x ->
          List.for_all b ~f:(fun y ->
            Ordering.neq (Key.compare (Value.key x) (Value.key y))))
    end)

  module Unordered = Make(struct
      type t = Value.t Key.Map.t

      let singleton x = Key.Map.singleton (Value.key x) x

      let union l =
        List.fold_left l ~init:Key.Map.empty ~f:(fun acc t ->
          Key.Map.merge acc t ~f:(fun _name x y ->
            match x, y with
            | Some x, _ | _, Some x -> Some x
            | _ -> None))

      let diff a b =
        Key.Map.merge a b ~f:(fun _name x y ->
          match x, y with
          | Some _, None -> x
          | _ -> None)
    end)

  type value = Value.t
  type 'a map = 'a Key.Map.t

  let eval t ~parse ~standard =
    if is_standard t then
      standard (* inline common case *)
    else
      Ordered.eval t ~parse
        ~special_values:(String.Map.singleton "standard" standard)

  let eval_unordered t ~parse ~standard =
    if is_standard t then
      standard (* inline common case *)
    else
      Unordered.eval t ~parse
        ~special_values:(String.Map.singleton "standard" standard)
end

let standard =
  { ast = Ast.Special (Loc.none, "standard")
  ; loc = None
  ; context = Univ_map.empty
  }

let field ?(default=standard) name = Sexp.Of_sexp.field name t ~default

let string_with_vars_no_dune fn =
  let open Sexp.Of_sexp in
  Syntax.renamed_in Stanza.syntax (1, 0)
    ~to_:(sprintf "Use %%{read-lines:%s} with one element per line"
            (fn |> Usexp.Ast.remove_locs |> Usexp.to_string))
  >>= fun () ->
  String_with_vars.t

module Unexpanded = struct
  type ast = (Sexp.Ast.t, Ast.unexpanded) Ast.t
  type t = ast generic
  let t =
    let open Sexp.Of_sexp in
    context >>= fun context ->
    raw >>| fun sexp ->
    let rec map (t : (Sexp.Ast.t, Ast.expanded) Ast.t) =
      let open Ast in
      match t with
      | Element x -> Element x
      | Union [Special (_, "include"); Element fn] ->
        Include (Sexp.Of_sexp.parse (string_with_vars_no_dune fn) context fn)
      | Union [Special (loc, "include"); _]
      | Special (loc, "include") ->
        Loc.fail loc "(:include expects a single element (do you need to quote the filename?)"
      | Special (l, s) -> Special (l, s)
      | Union l ->
        Union (List.map l ~f:map)
      | Diff (l, r) ->
        Diff (map l, map r)
    in
    { ast = map (parse_general sexp ~f:(fun x -> x))
    ; loc = Some (Sexp.Ast.loc sexp)
    ; context
    }

  let sexp_of_t t =
    let open Ast in
    let rec loop : ast -> Sexp.t = function
      | Element sexp -> Usexp.Ast.remove_locs sexp
      | Special (_, s) -> Sexp.atom (":" ^ s)
      | Union l -> List (List.map l ~f:loop)
      | Diff (a, b) -> List [loop a; Sexp.unsafe_atom_of_string "\\"; loop b]
      | Include fn -> List [ Sexp.unsafe_atom_of_string ":include"
                           ; String_with_vars.sexp_of_t fn
                           ]
    in
    loop t.ast

  let standard = standard

  let field ?(default=standard) name = Sexp.Of_sexp.field name t ~default

  type dune =
    { read: String.Set.t
    ; read_lines : String.Set.t
    }

  type files =
    | Dune of dune
    | Jbuild of String.Set.t

  let files =
    let jbuild ~f =
      let rec loop acc (t : ast) =
        let open Ast in
        match t with
        | Element _
        | Special _ -> acc
        | Include fn -> String.Set.add acc (f fn)
        | Union l ->
          List.fold_left l ~init:acc ~f:loop
        | Diff (l, r) ->
          loop (loop acc l) r
      in
      loop String.Set.empty
    in
    let dune =
      let loop_part acc (t : Usexp.Template.part) =
        match t with
        | Var { name="read-lines"; payload; _ } ->
          { acc with read_lines = String.Set.add acc.read_lines payload }
        | Var { name="read"; payload; _ } ->
          { acc with read_lines = String.Set.add acc.read payload }
        | Text _
        | Var _ -> acc
      in
      let rec loop_ast acc (t : Usexp.Ast.t) =
        match t with
        | Template t -> List.fold_left ~init:acc ~f:loop_part t.parts
        | List (_, xs) -> List.fold_left ~init:acc ~f:loop_ast xs
        | Atom (_, _)
        | Quoted_string (_, _) -> acc
      in
      let rec loop acc (t : ast) =
        let open Ast in
        match t with
        | Element e -> loop_ast acc e
        | Special _ -> acc
        | Include _ -> assert false
        | Union l ->
          List.fold_left l ~init:acc ~f:loop
        | Diff (l, r) ->
          loop (loop acc l) r
      in
      loop { read = String.Set.empty; read_lines = String.Set.empty }
    in
    fun t ~f ->
      match syntax t.context with
      | Dune -> Dune (dune t.ast)
      | Jbuild -> Jbuild (jbuild t.ast ~f)

  let has_special_forms t =
    let rec loop (t : ast) =
      let open Ast in
      match t with
      | Special _ | Include _ -> true
      | Element _ -> false
      | Union l ->
        List.exists l ~f:loop
      | Diff (l, r) ->
        loop l ||
        loop r
    in
    loop t.ast

  let expand t ~files_contents ~f  =
    let context = t.context in
    let rec expand (t : ast) : ast_expanded =
      let open Ast in
      match t with
      | Element s ->
        Element ( Sexp.Ast.loc s
                , f (Sexp.Of_sexp.parse String_with_vars.t context s)
                )
      | Special (l, s) -> Special (l, s)
      | Include fn ->
        let sexp =
          let fn = f fn in
          match String.Map.find files_contents fn with
          | Some x -> x
          | None ->
            Exn.code_error
              "Ordered_set_lang.Unexpanded.expand"
              [ "included-file", Quoted_string fn
              ; "files", Sexp.To_sexp.(list string)
                           (String.Map.keys files_contents)
              ]
        in
        parse_general sexp ~f:(fun sexp ->
          (Sexp.Ast.loc sexp,
           f (Sexp.Of_sexp.parse String_with_vars.t context sexp)))
      | Union l -> Union (List.map l ~f:expand)
      | Diff (l, r) ->
        Diff (expand l, expand r)
    in
    { t with ast = expand t.ast }
end

module String = Make(struct
    type t = string
    let compare = String.compare
    module Map = String.Map
  end)(struct
    type t = string
    type key = string
    let key x = x
  end)
