type syntax = Dollar_brace | Dollar_paren | Percent

type var =
  { loc: Loc.t
  ; name: string
  ; payload: string
  ; syntax: syntax
  }

type part =
  | Text of string
  | Var of var

type t =
  { quoted: bool
  ; parts: part list
  ; loc: Loc.t
  }

let to_string { parts ; _ } =
  let b = Buffer.create 16 in
  let with_syntax ~f = function
    | Percent ->
      Buffer.add_string b "%{";
      f ();
      Buffer.add_string b "}"
    | Dollar_brace ->
      Buffer.add_string b "${";
      f ();
      Buffer.add_string b "}"
    | Dollar_paren ->
      Buffer.add_string b "$(";
      f ();
      Buffer.add_string b ")" in
  let rec add_parts = function
    | [] -> ()
    | Text s:: xs ->
      Buffer.add_string b s; (* TODO escape here *)
      add_parts xs
    | Var { loc = _; syntax; name; payload } :: parts ->
      with_syntax syntax ~f:(fun () ->
        if name <> "" then begin
          Buffer.add_string b name;
          Buffer.add_char b ':';
        end;
        Buffer.add_string b payload (* TODO escape here *)
      );
      add_parts parts
  in
  add_parts parts;
  Buffer.contents b
;;

let sexp_of_t t =
  if t.quoted then
    Sexp.Quoted_string (to_string t)
  else
    Sexp.atom_or_quoted_string (to_string t)

let syntax_to_string = function
  | Dollar_brace -> "${}"
  | Dollar_paren -> "$()"
  | Percent -> "%{}"

let to_debug_sexp t =
  let a s = Sexp.Atom (Atom.of_string s) in
  let rec loop = function
    | [] -> []
    | Text s :: parts -> (Sexp.List [a "text"; Quoted_string s]) :: loop parts
    | Var { name; payload; syntax; loc=_ } :: parts ->
      (List
         [ List [a "name"; Quoted_string name]
         ; List [a "payload"; Quoted_string payload]
         ; List [a "syntax"; Quoted_string (syntax_to_string syntax) ]
         ]) :: loop parts in
  Sexp.List [a "template"; List (loop t.parts)]
