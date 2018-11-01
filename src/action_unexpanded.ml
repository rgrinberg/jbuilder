open! Stdune
open Import

include Action_dune_lang

module Unresolved = Action.Unresolved

module Mapper = Action.Make_mapper(Action_dune_lang)(Action_dune_lang)

let ignore_loc k ~loc:_ = k

let remove_locs =
  let no_loc_template = String_with_vars.make_text Loc.none "" in
  fun t ->
    Mapper.map t ~dir:no_loc_template
      ~f_program:(fun ~dir:_ -> String_with_vars.remove_locs)
      ~f_path:(fun ~dir:_ -> String_with_vars.remove_locs)
      ~f_string:(fun ~dir:_ -> String_with_vars.remove_locs)

let check_mkdir loc path =
  if not (Path.is_managed path) then
    Errors.fail loc
      "(mkdir ...) is not supported for paths outside of the workspace:\n\
      \  %a\n"
      (Dune_lang.pp Dune)
      (List [Dune_lang.unsafe_atom_of_string "mkdir"; Path_dune_lang.encode path])

module P : sig
  type 'a t

  val return : 'a -> 'a t

  val of_partial
    :  String_with_vars.t
    -> mode:'a String_with_vars.Mode.t
    -> expander:Expander.t
    -> 'a t

  val expand : 'a t -> Expander.t -> 'a

  val map_m : 'a list -> f:('a -> 'b t) -> 'b list t

  val is_expanded : _ t -> bool

  val expanded : 'a t -> 'a option

  module O : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  end
end = struct
  type 'a t =
    | Expanded of 'a
    | Unexpanded of (Expander.t -> 'a t)

  let expanded = function
    | Expanded a -> Some a
    | Unexpanded _ -> None

  let is_expanded = function
    | Expanded _ -> true
    | Unexpanded _ -> false

  let rec expand t expander =
    match t with
    | Expanded a -> a
    | Unexpanded a -> expand (a expander) expander

  let return a = Expanded a

  let of_partial template ~mode ~expander =
    let dir = Expander.dir expander in
    let f = Expander.expand_var_exn expander in
    match String_with_vars.partial_expand template ~mode ~dir ~f with
    | Expanded a -> Expanded a
    | Unexpanded a ->
      Unexpanded (fun expander ->
        let dir = Expander.dir expander in
        let f = Expander.expand_var_exn expander in
        Expanded (String_with_vars.expand ~mode ~dir ~f a))

  module O = struct
    let rec (>>=) t f =
      match t with
      | Expanded a -> f a
      | Unexpanded f' -> Unexpanded (fun e -> f' e >>= f)

    let (>>|) t f = t >>= fun x -> return (f x)
  end

  open O

  let rec map_m t ~f =
    match t with
    | [] -> Expanded []
    | [x] -> f x >>| fun x -> [x]
    | x :: xs ->
      f x >>= fun x ->
      map_m xs ~f >>| fun xs ->
      x :: xs
end

module Program = Unresolved.Program
module Partial = struct

  type t = Action.Unresolved.t P.t

  module E = struct
    let expand ~expander ~mode ~l ~r =
      Either.map ~l
        ~r:(fun s ->
          let dir = Expander.dir expander in
          r ~loc:(Some (String_with_vars.loc s))
            (Expander.expand expander ~template:s ~mode) ~dir)

    let string =
      expand ~mode:Single
        ~l:(fun x -> x)
        ~r:(ignore_loc Value.to_string)

    let strings =
      expand ~mode:Many
        ~l:(fun x -> [x])
        ~r:(ignore_loc Value.L.to_strings)

    let path e =
      let error_loc =
        match e with
        | Left _ -> None
        | Right r -> Some (String_with_vars.loc r) in
      expand ~mode:Single
        ~l:(fun x -> x)
        ~r:(ignore_loc (Value.(to_path ?error_loc))) e

    let prog_and_args_of_values ~loc p ~dir =
      match p with
      | [] -> (Unresolved.Program.Search (loc, ""), [])
      | Value.Dir p :: _ ->
        die "%s is a directory and cannot be used as an executable"
          (Path.to_string_maybe_quoted p)
      | Value.Path p :: xs -> (This p, Value.L.to_strings ~dir xs)
      | String s :: xs ->
        ( Unresolved.Program.of_string ~loc ~dir s
        , Value.L.to_strings ~dir xs
        )

    let prog_and_args =
      expand ~mode:Many
        ~l:(fun x -> (x, []))
        ~r:prog_and_args_of_values
  end

  let expand t ~expander : Unresolved.t = P.expand t expander
end

module E = struct
  let expand ~expander ~mode ~map x =
    let open P.O in
    let dir = Expander.dir expander in
    P.of_partial x ~mode ~expander >>| fun e ->
    let loc = Some (String_with_vars.loc x) in
    map ~loc e ~dir

  let string = expand ~mode:Single ~map:(ignore_loc Value.to_string)
  let strings = expand ~mode:Many ~map:(ignore_loc Value.L.to_strings)
  let cat_strings = expand ~mode:Many ~map:(ignore_loc Value.L.concat)
  let path x =
    expand ~mode:Single ~map:(fun ~loc v ~dir ->
      Value.to_path ?error_loc:loc v ~dir) x
  let prog_and_args = expand ~mode:Many ~map:Partial.E.prog_and_args_of_values
end

let rec partial_expand (t : t) ~map_exe ~expander : Partial.t =
  let open P.O in
  let open Unresolved in
  match t with
  | Run (prog, args) ->
    P.map_m args ~f:(E.strings ~expander)
    >>| List.concat
    >>= fun args ->
    E.prog_and_args ~expander prog >>| (fun (prog, more_args) ->
      let prog =
        match prog with
        | Search _ -> prog
        | This path -> This (map_exe path)
      in
      let args = more_args @ args in
      Run (prog, args))
  | Chdir (fn, t) -> begin
      let res = E.path ~expander fn in
      if P.is_expanded res then
        res >>= fun dir ->
        let expander = Expander.set_dir expander ~dir in
        partial_expand t ~expander ~map_exe >>| fun t ->
        Chdir (dir, t)
      else
        let loc = String_with_vars.loc fn in
        Errors.fail loc
          "This directory cannot be evaluated statically.\n\
           This is not allowed by dune"
    end
  | Setenv (var, value, t) ->
    (let var' = E.string ~expander var in
     if P.is_expanded var' then
       var'
     else
       Errors.fail (String_with_vars.loc var)
         "environment variable names must be static")
    >>= fun var ->
    E.string ~expander value >>= fun value ->
    let expander = Expander.set_env expander ~var ~value in
    partial_expand t ~expander ~map_exe >>| fun t ->
    Setenv (var, value, t)
  | Redirect (outputs, fn, t) ->
    E.path ~expander fn >>= fun fn ->
    partial_expand t ~expander ~map_exe >>| fun t ->
    Redirect (outputs, fn, t)
  | Ignore (outputs, t) ->
    partial_expand t ~expander ~map_exe >>| fun t ->
    Ignore (outputs, t)
  | Progn l ->
    P.map_m l ~f:(partial_expand ~map_exe ~expander)
    >>| fun l -> Progn l
  | Echo xs ->
    P.map_m xs ~f:(E.cat_strings ~expander)
    >>| fun xs -> Echo xs
  | Cat x ->
    E.path ~expander x
    >>| fun x -> Cat x
  | Copy (x, y) ->
    E.path ~expander x >>= fun x ->
    E.path ~expander y >>| fun y ->
    Copy (x, y)
  | Symlink (x, y) ->
    E.path ~expander x >>= fun x ->
    E.path ~expander y >>| fun y ->
    Symlink (x, y)
  | Copy_and_add_line_directive (x, y) ->
    E.path ~expander x >>= fun x ->
    E.path ~expander y >>| fun y ->
    Copy_and_add_line_directive (x, y)
  | System x ->
    E.string ~expander x >>| fun x ->
    System x
  | Bash x ->
    E.string ~expander x >>| fun x ->
    Bash x
  | Write_file (x, y) ->
    E.path ~expander x >>= fun x ->
    E.string ~expander y >>| fun y ->
    Write_file (x, y)
  | Rename (x, y) ->
    E.path ~expander x >>= fun x ->
    E.path ~expander y >>| fun y ->
    Rename (x, y)
  | Remove_tree x ->
    E.path ~expander x >>| fun x ->
    Remove_tree x
  | Mkdir x ->
    E.path ~expander x >>| (fun path ->
      check_mkdir (String_with_vars.loc x) path;
      Mkdir path)
  | Digest_files x ->
    P.map_m x ~f:(E.path ~expander)
    >>| fun x -> Digest_files x
  | Diff { optional; file1; file2; mode } ->
    E.path ~expander file1 >>= fun file1 ->
    E.path ~expander file2 >>| fun file2 ->
    Diff { optional ; file1 ; file2 ; mode}
  | Merge_files_into (sources, extras, target) ->
    P.map_m sources ~f:(E.path ~expander) >>= fun sources ->
    P.map_m extras ~f:(E.string ~expander) >>= fun extras ->
    E.path ~expander target >>| fun target ->
    Merge_files_into (sources, extras, target)

module Infer = struct
  module Outcome = struct
    type t =
      { deps    : Path.Set.t
      ; targets : Path.Set.t
      }
  end
  open Outcome

  module type Pset = sig
    type t
    val empty : t
    val diff : t -> t -> t
  end

  module type Outcome = sig
    type path_set
    type t =
      { deps    : path_set
      ; targets : path_set
      }
  end

  module type Primitives = sig
    type path
    type program
    type outcome
    val ( +@ ) : outcome -> path -> outcome
    val ( +< ) : outcome -> path -> outcome
    val ( +<! ) : outcome -> program -> outcome
  end

  module Make
      (Ast : Action_intf.Ast)
      (Pset : Pset)
      (Out : Outcome with type path_set := Pset.t)
      (Prim : Primitives
       with type path := Ast.path
       with type program := Ast.program
       with type outcome := Out.t) =
  struct
    open Ast
    open Out
    open Prim
    let rec infer acc t =
      match t with
      | Run (prog, _) -> acc +<! prog
      | Redirect (_, fn, t)  -> infer (acc +@ fn) t
      | Cat fn               -> acc +< fn
      | Write_file (fn, _)  -> acc +@ fn
      | Rename (src, dst)    -> acc +< src +@ dst
      | Copy (src, dst)
      | Copy_and_add_line_directive (src, dst)
      | Symlink (src, dst) -> acc +< src +@ dst
      | Chdir (_, t)
      | Setenv (_, _, t)
      | Ignore (_, t) -> infer acc t
      | Progn l -> List.fold_left l ~init:acc ~f:infer
      | Digest_files l -> List.fold_left l ~init:acc ~f:(+<)
      | Diff { optional; file1; file2; mode = _ } ->
        if optional then acc else acc +< file1 +< file2
      | Merge_files_into (sources, _extras, target) ->
        List.fold_left sources ~init:acc ~f:(+<) +@ target
      | Echo _
      | System _
      | Bash _
      | Remove_tree _
      | Mkdir _ -> acc

    let infer t =
      let { deps; targets } =
        infer { deps = Pset.empty; targets = Pset.empty } t
      in
      (* A file can be inferred as both a dependency and a target,
         for instance:

         {[
           (progn (copy a b) (copy b c))
         ]}
      *)
      { deps = Pset.diff deps targets; targets }
  end [@@inline always]

  include Make(Action)(Path.Set)(Outcome)(struct
      let ( +@ ) acc fn = { acc with targets = Path.Set.add acc.targets fn }
      let ( +< ) acc fn = { acc with deps    = Path.Set.add acc.deps    fn }
      let ( +<! ) acc prog =
        match prog with
        | Ok p -> acc +< p
        | Error _ -> acc
    end)

  module Partial_with_all_targets = Make(Partial.Past)(Path.Set)(Outcome)(struct
      let ( +@ ) acc fn =
        match fn with
        | Left  fn -> { acc with targets = Path.Set.add acc.targets fn }
        | Right sw ->
          Errors.fail (String_with_vars.loc sw)
            "Cannot determine this target statically."
      let ( +< ) acc fn =
        match fn with
        | Left  fn -> { acc with deps    = Path.Set.add acc.deps fn }
        | Right _  -> acc
      let ( +<! ) acc fn =
        match (fn : Partial.program) with
        | Left  (This fn) -> { acc with deps = Path.Set.add acc.deps fn }
        | Left  (Search _) | Right _ -> acc
    end)

  module Partial = Make(Partial.Past)(Path.Set)(Outcome)(struct
      let ( +@ ) acc fn =
        match fn with
        | Left  fn -> { acc with targets = Path.Set.add acc.targets fn }
        | Right _  -> acc
      let ( +< ) acc fn =
        match fn with
        | Left  fn -> { acc with deps    = Path.Set.add acc.deps fn }
        | Right _  -> acc
      let ( +<! ) acc fn =
        match (fn : Partial.program) with
        | Left  (This fn) -> { acc with deps = Path.Set.add acc.deps fn }
        | Left  (Search _) | Right _ -> acc
    end)

  let partial ~all_targets t =
    if all_targets then
      Partial_with_all_targets.infer t
    else
      Partial.infer t

  module S_unexp = struct
    type t = String_with_vars.t list
    let empty = []
    let diff a _ = a
  end

  module Outcome_unexp = struct
    type t =
      { deps    : S_unexp.t
      ; targets : S_unexp.t
      }
  end

  module Unexp = Make(Action_dune_lang)(S_unexp)(Outcome_unexp)(struct
      open Outcome_unexp
      let ( +@ ) acc fn =
        if String_with_vars.is_var fn ~name:"null" then
          acc
        else
          { acc with targets = fn :: acc.targets }
      let ( +< ) acc _ = acc
      let ( +<! )= ( +< )
    end)

  let unexpanded_targets t =
    (Unexp.infer t).targets
end

