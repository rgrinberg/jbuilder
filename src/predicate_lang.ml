open Stdune

module Ast = struct
  type 'a t =
    | Element of 'a
    | Standard
    | All
    | Union of 'a t list
    | Diff of 'a t * 'a t

  let decode elt =
    let open Stanza.Decoder in
    let elt = located elt >>| fun (loc, e) -> Element (loc, e) in
    let rec one (kind : Dune_lang.Syntax.t) =
      peek_exn >>= function
      | Atom (loc, A "\\") -> Errors.fail loc "unexpected \\"
      | (Atom (_, A "") | Quoted_string (_, _)) | Template _ ->
        elt
      | Atom (loc, A s) -> begin
          match s with
          | ":standard" ->
            junk >>> return Standard
          | ":all" ->
            junk >>> return All
          | ":include" ->
            Errors.fail loc ":include isn't supported in the predicate language"
          | _ when s.[0] = ':' ->
            Errors.fail loc "undefined symbol %s" s
          | _ ->
            elt
        end
      | List (_, Atom (loc, A s) :: _) -> begin
          match s, kind with
          | ":include", _ ->
            Errors.fail loc ":include isn't supported in the predicate language"
          | s, Dune when s <> "" && s.[0] <> '-' && s.[0] <> ':' ->
            Errors.fail loc
              "This atom must be quoted because it is the first element \
               of a list and doesn't start with - or :"
          | _ -> enter (many [] kind)
        end
      | List _ -> enter (many [] kind)
    and many acc kind =
      peek >>= function
      | None -> return (Union (List.rev acc))
      | Some (Atom (_, A "\\")) ->
        junk >>> many [] kind >>| fun to_remove ->
        Diff (Union (List.rev acc), to_remove)
      | Some _ ->
        one kind >>= fun x ->
        many (x :: acc) kind
    in
    Stanza.file_kind () >>= fun kind ->
    match kind with
    | Dune -> many [] kind
    | Jbuild -> one kind
end

type t =
  { ast : (Loc.t * Glob.t) Ast.t
  ; loc : Loc.t
  }

let decode =
  let open Stanza.Decoder in
  let%map (loc, ast) = located (Ast.decode Glob.decode) in
  { ast
  ; loc
  }

let empty =
  { ast = Union []
  ; loc = Loc.none
  }

let rec mem t ~standard ~elem =
  match (t : _ Ast.t) with
  | Element (_loc, a) -> Glob.test a elem
  | All -> true
  | Union xs -> List.exists ~f:(mem ~standard ~elem) xs
  | Diff (l, r) ->
    mem l ~standard ~elem && not (mem ~standard ~elem r)
  | Standard -> mem standard ~standard ~elem

let filter t ~standard elems =
  match t.ast with
  | All -> Lazy.force elems
  | Union [] -> []
  | _ ->
    (List.filter (Lazy.force elems)
       ~f:(fun elem -> mem t.ast ~standard:standard.ast ~elem))

