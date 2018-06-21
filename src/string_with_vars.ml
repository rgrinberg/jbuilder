open! Import

type var_syntax = Parens | Braces

type item =
  | Text of string
  | Var of var_syntax * string

type t =
  { items : item list
  ; loc : Loc.t
  ; quoted : bool }

module Token = struct
  type t =
    | String of string
    | Open   of var_syntax
    | Close  of var_syntax

  let tokenise s =
    let len = String.length s in
    let sub i j = String.sub s ~pos:i ~len:(j - i) in
    let cons_str i j acc = if i = j then acc else String (sub i j) :: acc in
    let rec loop i j =
      if j = len
      then cons_str i j []
      else
        match s.[j] with
        | '}' -> cons_str i j (Close Braces :: loop (j + 1) (j + 1))
        | ')' -> cons_str i j (Close Parens :: loop (j + 1) (j + 1))
        | '$' when j + 1 < len -> begin
            match s.[j + 1] with
            | '{' -> cons_str i j (Open Braces :: loop (j + 2) (j + 2))
            | '(' -> cons_str i j (Open Parens :: loop (j + 2) (j + 2))
            | _   -> loop i (j + 1)
          end
        | _ -> loop i (j + 1)
    in
    loop 0 0

  let to_string = function
    | String s     -> s
    | Open  Braces -> "${"
    | Open  Parens -> "$("
    | Close Braces -> "}"
    | Close Parens -> ")"
end

(* Remark: Consecutive [Text] items are concatenated. *)
let rec of_tokens : Token.t list -> item list = function
  | [] -> []
  | Open a :: String s :: Close b :: rest when a = b ->
    Var (a, s) :: of_tokens rest
  | token :: rest ->
    let s = Token.to_string token in
    match of_tokens rest with
    | Text s' :: l -> Text (s ^ s') :: l
    | l -> Text s :: l

let items_of_string s = of_tokens (Token.tokenise s)

let t =
  let open Sexp.Of_sexp in
  Syntax.get_exn Stanza.syntax >>= fun (_ : (int * int)) ->
  raw >>| fun sexp ->
  match sexp with
  | Atom(loc, A s) -> { items = items_of_string s;  loc;  quoted = false }
  | Quoted_string (loc, s) ->
    { items = items_of_string s;  loc;  quoted = true }
  | List (loc, _) -> of_sexp_error loc "Atom or quoted string expected"

let loc t = t.loc

let virt ?(quoted=false) pos s =
  { items = items_of_string s;  loc = Loc.of_pos pos;  quoted }
let virt_var ?(quoted=false) pos s =
  { items = [Var (Braces, s)];  loc = Loc.of_pos pos;  quoted }
let virt_text pos s =
  { items = [Text s];  loc = Loc.of_pos pos;  quoted = true }

let sexp_of_var_syntax = function
  | Parens -> Sexp.unsafe_atom_of_string "parens"
  | Braces -> Sexp.unsafe_atom_of_string "braces"

let sexp_of_item =
  let open Sexp in function
    | Text s -> List [Sexp.unsafe_atom_of_string "text" ;
                      Sexp.atom_or_quoted_string s]
    | Var (vs, s) -> List [sexp_of_var_syntax vs ;
                           Sexp.atom_or_quoted_string s]

let sexp_of_ast t = Sexp.To_sexp.list sexp_of_item t.items

let fold t ~init ~f =
  List.fold_left t.items ~init ~f:(fun acc item ->
    match item with
    | Text _ -> acc
    | Var (_, v) -> f acc t.loc v)

let iter t ~f = List.iter t.items ~f:(function
  | Text _ -> ()
  | Var (_, v) -> f t.loc v)

let vars t = fold t ~init:String.Set.empty ~f:(fun acc _ x -> String.Set.add acc x)

let string_of_var syntax v =
  match syntax with
  | Parens -> sprintf "$(%s)" v
  | Braces -> sprintf "${%s}" v

let concat_rev = function
  | [] -> ""
  | [s] -> s
  | l -> String.concat (List.rev l) ~sep:""

module Mode = struct
  type 'a t =
    | Single : Value.t t
    | Many : Value.t list t

  let string
    : type a. a t -> string -> a
    = fun t s ->
      match t with
      | Single -> Value.String s
      | Many -> [Value.String s]

  let value
    : type a. a t -> Value.t list -> a option
    = fun t s ->
      match t, s with
      | Many, s -> Some s
      | Single, [s] -> Some s
      | Single, _ -> None
end

module Partial = struct
  type nonrec 'a t =
    | Expanded of 'a
    | Unexpanded of t
end

let invalid_multivalue syntax ~var t x =
  Loc.fail t.loc "Variable %s expands to %d values, \
                  however a single value is expected here. \
                  Please quote this atom."
    (string_of_var syntax var) (List.length x)

let partial_expand t ~mode ~dir ~f =
  let commit_text acc_text acc =
    let s = concat_rev acc_text in
    if s = "" then acc else Text s :: acc
  in
  let rec loop acc_text acc items =
    match items with
    | [] ->
      begin match acc with
      | [] -> Partial.Expanded (Mode.string mode (concat_rev acc_text))
      | _  -> Unexpanded { t with items = List.rev (commit_text acc_text acc) }
      end
    | Text s :: items -> loop (s :: acc_text) acc items
    | Var (syntax, var) as it :: items ->
      begin match f syntax t.loc var with
      | Some ([] | _::_::_ as e) when not t.quoted ->
        invalid_multivalue syntax ~var t e
      | Some t ->
        loop (Value.L.concat ~dir t :: acc_text) acc items
      | None -> loop [] (it :: commit_text acc_text acc) items
      end
  in
  match t.items with
  | [] -> Partial.Expanded (Mode.string mode "")
  | [Text s] -> Expanded (Mode.string mode s)
  | [Var (syntax, v)] when not t.quoted ->
    (* Unquoted single var *)
    begin match f syntax t.loc v with
    | Some e -> Partial.Expanded (
      match Mode.value mode e with
      | None -> invalid_multivalue syntax ~var:v t e
      | Some s -> s)
    | None -> Unexpanded t
    end
  | _ -> loop [] [] t.items

let expand t ~mode ~dir ~f =
  match
    partial_expand t ~mode ~dir ~f:(fun syntax loc var ->
      match f loc var with
      | None -> Some [Value.String (string_of_var syntax var)]
      | s -> s)
  with
  | Partial.Expanded s -> s
  | Unexpanded _ -> assert false (* we are expanding every variable *)

let partial_expand t ~mode ~dir ~f =
  partial_expand t ~mode ~dir ~f:(fun _ loc v -> f loc v)

let to_string t =
  match t.items with
  (* [to_string is only called from action.ml, always on [t]s of this form *)
  | [Var (syntax, v)] -> string_of_var syntax v
  | items ->
    List.map items ~f:(function
      | Text s -> s
      | Var (syntax, v) -> string_of_var syntax v)
    |> String.concat ~sep:""

let sexp_of_t t = Sexp.To_sexp.string (to_string t)

let is_var t ~name =
  match t.items with
  | [Var (_, v)] -> v = name
  | _ -> false
