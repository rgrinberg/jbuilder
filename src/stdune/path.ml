let explode_path =
  let rec loop path acc =
    let dir  = Filename.dirname  path in
    let base = Filename.basename path in
    if dir = Filename.current_dir_name then
      base :: acc
    else if dir = path then
      acc
    else
      loop dir (base :: acc)
  in
  fun path -> loop path []

module External : sig
  type t

  val compare : t -> t -> Ordering.t
  val compare_val : t -> t -> Ordering.t
  val t : t Sexp.Of_sexp.t
  val sexp_of_t : t Sexp.To_sexp.t
  val to_string : t -> string
  val of_string : string -> t
  val relative : t -> string -> t
  val mkdir_p : t -> unit
  val basename : t -> string
  val parent : t -> t
  val initial_cwd : t
  val cwd : unit -> t
  val extend_basename : t -> suffix:string -> t
end = struct
  include Interned.Make(struct
      let initial_size = 512
      let resize_policy = Interned.Greedy
    end)

  let compare_val x y = String.compare (to_string x) (to_string y)

  let as_string x ~f =
    to_string x
    |> f
    |> make

  let extend_basename t ~suffix = as_string t ~f:(fun t -> t ^ suffix)

  let of_string t =
    if Filename.is_relative t then
      Exn.code_error "Path.External.of_string: relative path given"
        [ "t", Sexp.To_sexp.string t ];
    make t

  let sexp_of_t t = Sexp.To_sexp.string (to_string t)
  let t sexp =
    let t = Sexp.Of_sexp.string sexp in
    if Filename.is_relative t then
      Sexp.Of_sexp.of_sexp_error sexp "Absolute path expected";
    of_string t

(*
  let rec cd_dot_dot t =
    match Unix.readlink t with
    | exception _ -> Filename.dirname t
    | t -> cd_dot_dot t

  let relative initial_t path =
    let rec loop t components =
      match components with
      | [] | ["." | ".."] ->
        die "invalid filename concatenation: %s / %s" initial_t path
      | [fn] -> Filename.concat t fn
      | "."  :: rest -> loop t rest
      | ".." :: rest -> loop (cd_dot_dot t) rest
      | comp :: rest -> loop (Filename.concat t comp) rest
    in
    loop initial_t (explode_path path)
*)

  let relative x y = make (Filename.concat (to_string x) y)

  let rec mkdir_p t =
    let t_s = to_string t in
    let p_s = Filename.dirname t_s in
    let p = make p_s in
    if p <> t then
      try
        Unix.mkdir t_s 0o777
      with
      | Unix.Unix_error (EEXIST, _, _) -> ()
      | Unix.Unix_error (ENOENT, _, _) ->
        mkdir_p p;
        Unix.mkdir t_s 0o777

  let basename t = Filename.basename (to_string t)
  let parent t = as_string ~f:Filename.dirname t

  let cwd () = make (Sys.getcwd ())
  let initial_cwd = cwd ()
end

module Local : sig
  type t

  val t : t Sexp.Of_sexp.t
  val sexp_of_t : t Sexp.To_sexp.t
  val root : t
  val is_root : t -> bool
  val compare : t -> t -> Ordering.t
  val compare_val : t -> t -> Ordering.t
  val of_string : ?error_loc:Usexp.Loc.t -> string -> t
  val to_string : t -> string
  val relative : ?error_loc:Usexp.Loc.t -> t -> string -> t
  val append : t -> t -> t
  val parent : t -> t
  val mkdir_p : t -> unit
  val descendant : t -> of_:t -> t option
  val is_descendant : t -> of_:t -> bool
  val reach : t -> from:t -> t
  val basename : t -> string
  val extend_basename : t -> suffix:string -> t
  module Set : Set.S with type elt = t

  module Prefix : sig
    type local = t
    type t

    val make : local -> t

    (* for all local path p, drop (invalid p = None) *)
    val invalid : t
  end with type local := t

  val of_string'
    : ?error_loc:Usexp.Loc.t -> strip_prefix:Prefix.t -> string
    -> [`Prefix of t | `No_prefix of t]
end = struct
  (* either "" for root, either a '/' separated list of components
     other that ".", ".."  and not containing '/'. *)
  module Comp = Interned.Make(struct
      let initial_size = 512
      let resize_policy = Interned.Greedy
    end)

  module T = struct
    type t = Comp.t list
    let compare = List.compare ~compare:Comp.compare
  end

  include T

  module Set = Set.Make(T)

  let compare_val x y =
    List.compare (List.rev x) (List.rev y) ~compare:(fun x y ->
      String.compare (Comp.to_string x) (Comp.to_string y))

  let root = []

  let to_istring p =
    List.rev_map ~f:Comp.to_string p
    |> String.concat ~sep:"/"

  let is_root = function
    | [] -> true
    | _::_ -> false

  let to_string t = if is_root t then "." else to_istring t

  let parent = function
    | [] -> Exn.code_error "Path.Local.parent called on the root" []
    | _::xs -> xs

  let basename = function
    | [] -> Exn.code_error "Path.Local.basename called on the root" []
    | b::_ -> Comp.to_string b

  let sexp_of_t t = Sexp.To_sexp.string (to_istring t)

  let relative ?error_loc t path =
    if not (Filename.is_relative path) then (
      Exn.code_error "Local.relative: received absolute path"
        [ "t", sexp_of_t t
        ; "path", Usexp.atom_or_quoted_string path
        ]
    );
    let rec loop t components =
      match components with
      | [] -> Result.Ok t
      | "." :: rest -> loop t rest
      | ".." :: rest ->
        if is_root t then
          Result.Error ()
        else
          loop (parent t) rest
      | fn :: rest ->
        loop (Comp.make fn :: t) rest
    in
    match loop t (explode_path path) with
    | Result.Ok t -> t
    | Error () ->
      Exn.fatalf ?loc:error_loc "path outside the workspace: %s from %s" path
        (to_string t)
  ;;

  let relative' ~strip_prefix ?error_loc t path =
    if not (Filename.is_relative path) then (
      Exn.code_error "Local.relative: received absolute path"
        [ "t", sexp_of_t t
        ; "path", Usexp.atom_or_quoted_string path
        ]
    );
    let rec loop' t components =
      match components with
      | [] -> Result.Ok t
      | "." :: rest -> loop' t rest
      | ".." :: rest ->
        if is_root t then
          Result.Error ()
        else
          loop' (parent t) rest
      | fn :: rest ->
        loop' (Comp.make fn :: t) rest
    and loop t prefix components =
      match components, prefix with
      | [], _::_ -> Result.Ok (`No_prefix t)
      | [], [] -> Result.Ok (`Prefix t)
      | "." :: rest, _ -> loop t prefix rest
      | ".." :: rest, _ ->
        if is_root t then
          Result.Error ()
        else
          loop (parent t) prefix rest
      | fn :: rest, [] ->
        Result.map ~f:(fun x -> `No_prefix x)
          (loop' ((Comp.make fn) :: t) rest)
      | fn :: rest, p :: ps ->
        begin match Comp.compare (Comp.make fn) p with
        | Ordering.Eq -> loop (Comp.make fn :: t) ps rest
        | _ -> Result.map
                 ~f:(fun x -> `No_prefix x)
                 (loop' (Comp.make fn :: t) rest)
        end
    in
    match
      match strip_prefix with
      | Some strip_prefix -> loop t strip_prefix (explode_path path)
      | None -> Result.map ~f:(fun x -> `No_prefix x)
                  (loop' t (explode_path path))
    with
    | Result.Ok t -> t
    | Error () ->
      Exn.fatalf ?loc:error_loc "path outside the workspace: %s from %s" path
        (to_string t)

  let _is_canonicalized =
    let rec before_slash s i =
      if i < 0 then
        false
      else
        match s.[i] with
        | '/' -> false
        | '.' -> before_dot_slash s (i - 1)
        | _   -> in_component     s (i - 1)
    and before_dot_slash s i =
      if i < 0 then
        false
      else
        match s.[i] with
        | '/' -> false
        | '.' -> before_dot_dot_slash s (i - 1)
        | _   -> in_component         s (i - 1)
    and before_dot_dot_slash s i =
      if i < 0 then
        false
      else
        match s.[i] with
        | '/' -> false
        | _   -> in_component s (i - 1)
    and in_component s i =
      if i < 0 then
        true
      else
        match s.[i] with
        | '/' -> before_slash s (i - 1)
        | _   -> in_component s (i - 1)
    in
    fun s ->
      let len = String.length s in
      len = 0 || before_slash s (len - 1)

  let of_string ?error_loc s =
    relative root s ?error_loc

  let of_string' ?error_loc ~strip_prefix s =
    relative' root s ~strip_prefix ?error_loc

  let t sexp =
    of_string (Sexp.Of_sexp.string sexp)
      ~error_loc:(Sexp.Ast.loc sexp)

  let rec mkdir_p t =
    if is_root t then
      ()
    else
      let t_s = to_string t in
      try
        Unix.mkdir t_s 0o777
      with
      | Unix.Unix_error (EEXIST, _, _) -> ()
      | Unix.Unix_error (ENOENT, _, _) as e ->
        let parent = parent t in
        if is_root parent then
          raise e
        else begin
          mkdir_p parent;
          Unix.mkdir t_s 0o777
        end

  let append a b =
    match is_root a, is_root b with
    | true, _ -> b
    | _, true -> a
    | _, _ -> List.fold_right ~f:List.cons ~init:a b

  let descendant t ~of_ =
    let rec loop t of_ =
      match t, of_ with
      | t, [] -> Some (List.rev t)
      | [], _::_ -> None
      | x::xs, y::ys ->
        begin match Comp.compare x y with
        | Ordering.Eq -> loop xs ys
        | _ -> None
        end in
    match t, of_ with
    | t, []  -> Some t
    | [], _ -> None
    | x, y ->
      begin match compare x y with
      | Ordering.Eq -> Some x
      | _ -> loop (List.rev t) (List.rev of_)
      end

  let is_descendant t ~of_ = Option.is_some (descendant t ~of_)

  let reach t ~from =
    let rec loop t from =
      match t, from with
      | x::xs, y::ys when Comp.compare x y = Ordering.Eq ->
        loop xs ys
      | _, _ ->
        append (List.map ~f:(fun _ -> Comp.make "..") from) (List.rev t)
    in
    loop (List.rev t) (List.rev from)

  let extend_basename t ~suffix =
    match t with
    | [] -> [Comp.make suffix]
    | x::xs -> (Comp.make (Comp.to_string x ^ suffix)) :: xs

  module Prefix = struct
    type t = Comp.t list option
    let make p = Some (List.rev p)
    let invalid = None
  end
end

let (_abs_root, set_root) =
  let root_dir = ref None in
  let set_root new_root =
    match !root_dir with
    | None -> root_dir := Some new_root
    | Some root_dir ->
      Exn.code_error "set_root: cannot set root_dir more than once"
        [ "root_dir", External.sexp_of_t root_dir
        ; "new_root_dir", External.sexp_of_t new_root
        ]
  in
  let abs_root = lazy (
    match !root_dir with
    | None ->
      Exn.code_error "root_dir: cannot use root dir before it's set" []
    | Some root_dir -> root_dir)
  in
  (abs_root, set_root)

module Kind = struct
  type t =
    | External of External.t
    | Local    of Local.t

  let to_absolute_filename t ~root =
    match t with
    | External s -> External.to_string s
    | Local l -> External.to_string (External.relative root (Local.to_string l))

  let to_string = function
    | Local t -> Local.to_string t
    | External t -> External.to_string t

  let sexp_of_t t = Sexp.atom_or_quoted_string (to_string t)

  let of_string s =
    if s = "" || Filename.is_relative s then
      Local (Local.of_string s)
    else
      External (External.of_string s)

  let _relative ?error_loc t fn =
    match t with
    | Local t -> Local (Local.relative ?error_loc t fn)
    | External t -> External (External.relative t fn)

  let mkdir_p = function
    | Local t -> Local.mkdir_p t
    | External t -> External.mkdir_p t

  let append_local x y =
    match x with
    | Local x -> Local (Local.append x y)
    | External x -> External (External.relative x (Local.to_string y))

end

let (build_dir_kind, build_dir_prefix, set_build_dir) =
  let build_dir = ref None in
  let build_dir_prefix = ref None in
  let set_build_dir (new_build_dir : Kind.t) =
    match !build_dir with
    | None ->
      build_dir := Some new_build_dir;
      build_dir_prefix :=
        Some (match new_build_dir with
          | Local    p -> Local.Prefix.make p
          | External _ -> Local.Prefix.invalid)
    | Some build_dir ->
      Exn.code_error "set_build_dir: cannot set build_dir more than once"
        [ "build_dir", Kind.sexp_of_t build_dir
        ; "new_build_dir", Kind.sexp_of_t new_build_dir ]
  in
  let build_dir = lazy (
    match !build_dir with
    | None ->
      Exn.code_error "build_dir: cannot use build dir before it's set" []
    | Some build_dir -> build_dir)
  in
  let build_dir_prefix = lazy (
    match !build_dir_prefix with
    | None ->
      Exn.code_error "build_dir: cannot use build dir before it's set" []
    | Some prefix -> prefix)
  in
  (build_dir, build_dir_prefix, set_build_dir)

module T : sig
  type t = private
    | External of External.t
    | In_source_tree of Local.t
    | In_build_dir of Local.t

  val compare : t -> t -> Ordering.t

  val in_build_dir : Local.t -> t
  val in_source_tree : Local.t -> t
  val external_ : External.t -> t
end = struct
  type t =
    | External of External.t
    | In_source_tree of Local.t
    | In_build_dir of Local.t

  let compare x y =
    match x, y with
    | External x      , External y       -> External.compare x y
    | External _      , _                -> Lt
    | _               , External _       -> Gt
    | In_source_tree x, In_source_tree y -> Local.compare x y
    | In_source_tree _, _                -> Lt
    | _               , In_source_tree _ -> Gt
    | In_build_dir x  , In_build_dir y   -> Local.compare x y

  let in_build_dir s = In_build_dir s
  let in_source_tree s = In_source_tree s
  let external_ e = External e
end

include T

let build_dir = in_build_dir Local.root

let is_root = function
  | In_source_tree s -> Local.is_root s
  | In_build_dir _
  | External _  -> false

module Map = Map.Make(T)

let kind = function
  | In_build_dir p -> Kind.append_local (Lazy.force build_dir_kind) p
  | In_source_tree s -> Kind.Local s
  | External s -> Kind.External s

let is_local_fn t = t = "" || Filename.is_relative t

let is_managed = function
  | In_build_dir _
  | In_source_tree _ -> true
  | External _ -> false

let to_string t = Kind.to_string (kind t)

let to_string_maybe_quoted t =
  String.maybe_quoted (to_string t)

let root = in_source_tree Local.root

let relative ?error_loc t fn =
  if fn = "" then
    t
  else if not (Filename.is_relative fn) then
    external_ (External.of_string fn)
  else
    match t with
    | In_source_tree p ->
      if Local.is_root p then
        match Local.of_string' ~strip_prefix:(Lazy.force build_dir_prefix)
                fn ?error_loc
        with
        | `No_prefix p -> in_source_tree p
        | `Prefix p -> in_build_dir p
      else
        in_source_tree (Local.relative p fn ?error_loc)
    | In_build_dir p -> in_build_dir (Local.relative p fn ?error_loc)
    | External s -> external_ (External.relative s fn)

let of_string ?error_loc s =
  match s with
  | "" -> in_source_tree Local.root
  | s  ->
    if not (Filename.is_relative s) then
      external_ (External.of_string s)
    else
      match Local.of_string' ~strip_prefix:(Lazy.force build_dir_prefix)
              s ?error_loc
      with
      | `No_prefix p -> in_source_tree p
      | `Prefix p -> in_build_dir p

let t = function
  (* the first 2 cases are necessary for old build dirs *)
  | Sexp.Ast.Atom (_, A s)
  | Quoted_string (_, s) -> of_string s
  | s ->
    let open Sexp.Of_sexp in
    sum
      [ cstr "In_build_dir" (Local.t @> nil) in_build_dir
      ; cstr "In_source_tree" (Local.t @> nil) in_source_tree
      ; cstr "External" (External.t @> nil) external_
      ] s

let sexp_of_t t =
  let constr f x y = Sexp.To_sexp.(pair string f) (x, y) in
  match t with
  | In_build_dir s -> constr Local.sexp_of_t "In_build_dir" s
  | In_source_tree s -> constr Local.sexp_of_t "In_source_tree" s
  | External s -> constr External.sexp_of_t "External" s

let absolute fn =
  external_ (
    if is_local_fn fn then
      External.relative External.initial_cwd fn
    else
      External.of_string fn
  )

let to_absolute_filename t ~root = Kind.to_absolute_filename (kind t) ~root

let reach t ~from =
  match kind t, kind from with
  | External t, _ -> External.to_string t
  | (Local _) as l, External _ ->
    Kind.to_absolute_filename l ~root:External.initial_cwd
  | Local t, Local from -> Local.to_string (Local.reach t ~from)

let reach_for_running ?(from=root) t =
  match kind t, kind from with
  | External t, _ -> External.to_string t
  | Local _, External _ ->
    Exn.code_error "Path.reach_for_running called with invalid combination"
      [ "t"   , sexp_of_t t
      ; "from", sexp_of_t from
      ]
  | Local t, Local from ->
    let reach = Local.reach t ~from in
    let s = Local.to_string reach in
    if String.is_prefix s ~prefix:"../" then
      s
    else
      "./" ^ s

let descendant t ~of_ =
  match kind t, kind of_ with
  | Local t, Local of_ -> Option.map ~f:in_source_tree (Local.descendant t ~of_)
  | _, _ -> None

let is_descendant t ~of_ =
  match kind t, kind of_ with
  | Local t, Local of_ -> Local.is_descendant t ~of_
  | _, _ -> false

let append a b =
  match kind b with
  | External _ ->
    Exn.code_error "Path.append called with non-local second path"
      [ "a", sexp_of_t a
      ; "b", sexp_of_t b
      ]
  | Local b ->
    begin match a with
    | In_source_tree a -> in_source_tree (Local.append a b)
    | In_build_dir a -> in_build_dir (Local.append a b)
    | External a -> external_ (External.relative a (Local.to_string b))
    end

let basename t =
  match kind t with
  | Local t -> Local.basename t
  | External t -> External.basename t

let parent = function
  | External s ->
    let parent = External.parent s in
    if parent = s then
      None
    else
      Some (external_ parent)
  | In_source_tree p | In_build_dir p when Local.is_root p  -> None
  | In_source_tree l -> Some (in_source_tree (Local.parent l))
  | In_build_dir l -> Some (in_build_dir (Local.parent l))

let parent_exn t =
  match parent t with
  | Some p -> p
  | None -> Exn.code_error "Path.parent:exn t is root"
              ["t", sexp_of_t t]

let is_in_build_dir = function
  | In_build_dir p -> not (Local.is_root p)
  | In_source_tree _
  | External _ -> false

let is_in_source_tree = function
  | In_source_tree _ -> true
  | In_build_dir _
  | External _ -> false

let is_alias_stamp_file = function
  | In_build_dir s -> String.is_prefix (Local.to_string s) ~prefix:".aliases/"
  | In_source_tree _
  | External _ -> false

let extract_build_context = function
  | In_source_tree _
  | External _ -> None
  | In_build_dir p when Local.is_root p -> None
  | In_build_dir t ->
    let t = Local.to_string t in
    begin match String.index t '/' with
    | None ->
      Some ( String.sub t ~pos:0 ~len:(String.length t)
           , in_source_tree Local.root )
    | Some j ->
      Some
        ( String.sub t ~pos:0 ~len:j
        , String.sub t ~pos:(j + 1) ~len:(String.length t - j - 1)
          |> Local.of_string
          |> in_source_tree )
    end

let extract_build_context_dir = function
  | In_source_tree _
  | External _ -> None
  | In_build_dir t ->
    let t_str = Local.to_string t in
    begin match String.index t_str '/' with
    | None -> Some (in_build_dir t, in_source_tree Local.root)
    | Some j ->
      Some
        ( in_build_dir (Local.of_string (String.sub t_str ~pos:0 ~len:j))
        , (String.sub t_str ~pos:(j + 1) ~len:(String.length t_str - j - 1))
          |> Local.of_string
          |> in_source_tree
        )
    end

let drop_build_context t =
  Option.map (extract_build_context t) ~f:snd

let drop_build_context_exn t =
  match extract_build_context t with
  | None -> Exn.code_error "Path.drop_build_context_exn" [ "t", sexp_of_t t ]
  | Some (_, t) -> t

let drop_optional_build_context t =
  match extract_build_context t with
  | None -> t
  | Some (_, t) -> t

let split_first_component t =
  match kind t, is_root t with
  | Local t, false ->
    let t = Local.to_string t in
    begin match String.index t '/' with
    | None -> Some (t, root)
    | Some i ->
      Some
        ( String.sub t ~pos:0 ~len:i
        , String.sub t ~pos:(i + 1) ~len:(String.length t - i - 1)
          |> Local.of_string
          |> in_source_tree )
    end
  | _, _ -> None

let explode t =
  match kind t with
  | Local p when Local.is_root p -> Some []
  | Local s -> Some (String.split (Local.to_string s) ~on:'/')
  | External _ -> None

let explode_exn t =
  match explode t with
  | Some s -> s
  | None -> Exn.code_error "Path.explode_exn"
              ["path", sexp_of_t t]

let exists t =
  try Sys.file_exists (to_string t)
  with Sys_error _ -> false
let readdir t = Sys.readdir (to_string t) |> Array.to_list
let is_directory t =
  try Sys.is_directory (to_string t)
  with Sys_error _ -> false
let rmdir t = Unix.rmdir (to_string t)
let win32_unlink fn =
  try
    Unix.unlink fn
  with Unix.Unix_error (Unix.EACCES, _, _) as e ->
    (* Try removing the read-only attribute *)
    try
      Unix.chmod fn 0o666;
      Unix.unlink fn
    with _ ->
      raise e
let unlink_operation =
  if Sys.win32 then
    win32_unlink
  else
    Unix.unlink
let unlink t =
  unlink_operation (to_string t)
let unlink_no_err t = try unlink t with _ -> ()

let build_dir_exists () = is_directory build_dir

let ensure_build_dir_exists () =
  match kind build_dir with
  | Local p -> Local.mkdir_p p
  | External p ->
    let p = External.to_string p in
    try
      Unix.mkdir p 0o777
    with
    | Unix.Unix_error (EEXIST, _, _) -> ()
    | Unix.Unix_error (ENOENT, _, _) ->
      Exn.fatalf "Cannot create external build directory %s. \
                  Make sure that the parent dir %s exists."
        p (Filename.dirname p)

let extend_basename t ~suffix =
  match t with
  | In_source_tree t -> in_source_tree (Local.extend_basename t ~suffix)
  | In_build_dir t -> in_build_dir (Local.extend_basename t ~suffix)
  | External t -> external_ (External.extend_basename t ~suffix)

let insert_after_build_dir_exn =
  let error a b =
    Exn.code_error
      "Path.insert_after_build_dir_exn"
      [ "path"  , sexp_of_t a
      ; "insert", Sexp.unsafe_atom_of_string b
      ]
  in
  fun a b ->
    match a with
    | In_build_dir a -> in_build_dir (Local.append (Local.of_string b) a)
    | In_source_tree _
    | External _ -> error a b

let rm_rf =
  let rec loop dir =
    Array.iter (Sys.readdir dir) ~f:(fun fn ->
      let fn = Filename.concat dir fn in
      match Unix.lstat fn with
      | { st_kind = S_DIR; _ } -> loop fn
      | _                      -> unlink_operation fn);
    Unix.rmdir dir
  in
  fun t ->
    if not (is_managed t) then (
      Exn.code_error "Path.rm_rf called on external dir"
        ["t", sexp_of_t t]
    );
    let fn = to_string t in
    match Unix.lstat fn with
    | exception Unix.Unix_error(ENOENT, _, _) -> ()
    | _ -> loop fn

let mkdir_p = function
  | External s ->
    Exn.code_error "Path.mkdir_p cannot create external path"
      ["s", External.sexp_of_t s]
  | In_source_tree s ->
    Exn.code_error "Path.mkdir_p cannot dir in source"
      ["s", Local.sexp_of_t s]
  | In_build_dir k ->
    Kind.mkdir_p (Kind.append_local (Lazy.force build_dir_kind) k)

let compare_val x y =
  match x, y with
  | External x      , External y       -> External.compare_val x y
  | External _      , _                -> Lt
  | _               , External _       -> Gt
  | In_source_tree x, In_source_tree y -> Local.compare_val x y
  | In_source_tree _, _                -> Lt
  | _               , In_source_tree _ -> Gt
  | In_build_dir x  , In_build_dir y   -> Local.compare_val x y

let extension t = Filename.extension (to_string t)

let pp ppf t = Format.pp_print_string ppf (to_string t)

let pp_debug ppf = function
  | In_source_tree s ->
    Format.fprintf ppf "(In_source_tree %S)" (Local.to_string s)
  | In_build_dir s ->
    Format.fprintf ppf "(In_build_dir %S)" (Local.to_string s)
  | External s -> Format.fprintf ppf "(External %S)" (External.to_string s)

module Set = struct
  include Set.Make(T)
  let sexp_of_t t = Sexp.To_sexp.(list sexp_of_t) (to_list t)
  let of_string_set ss ~f =
    String.Set.to_list ss
    |> List.map ~f
    |> of_list

  let to_alpha_list t = List.sort (to_list t) ~compare:compare_val
end

let in_source s = in_source_tree (Local.of_string s)
