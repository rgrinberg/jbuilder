open! Import

include Sub_system_intf

module Register_backend(M : Backend) = struct
  type t = M.t

  include Jbuild.Sub_system_info.Register(M.Info)
  include Lib.Sub_system.Register(struct
      include M
      type Lib.Sub_system.t += T of t
      let to_sexp = Some to_sexp
    end)

  module Closure =
    Top_closure.Make(Int)
      (struct
        type t = M.t
        type graph = unit
        let key t = (M.id t).unique_id
        let deps t () = M.deps t
      end)

  module Set =
    Set.Make(struct
      type t = M.t
      let compare a b = compare (M.id a).unique_id (M.id b).unique_id
    end)

  let select_backends ~loc ~scope ~written_by_user to_scan =
    let open Result.O in
    let backends =
      match written_by_user with
      | Some l ->
        Result.all
          (List.map l ~f:(fun ((loc, name) as x) ->
             Lib.DB.resolve (Scope.libs scope) x >>= fun lib ->
             match get lib with
             | None ->
               Error (Loc.exnf loc "%S is not a %s" name M.desc)
             | Some t -> Ok t))
      | None ->
        Ok (List.filter_map to_scan ~f:get)
    in
    backends >>= function
    | [] ->
      Error
        (Loc.exnf loc "No %s found." M.desc)
    | backends ->
      Result.map_error (Closure.top_closure () backends) ~f:(fun cycle ->
        Loc.exnf loc
          "Dependency cycle between the following %s:\n    %s"
          M.desc
          (String.concat ~sep:"\n--> "
             (List.map cycle ~f:(fun t ->
                let id = M.id t in
                sprintf "%S in %s"
                  id.name (Path.to_string_maybe_quoted id.path)))))
      >>= fun backends ->
      let roots =
        let all = Set.of_list backends in
        List.fold_left backends ~init:all ~f:(fun acc t ->
          Set.diff acc (Set.of_list (M.deps t)))
      in
      if Set.cardinal roots = 1 then
        Ok backends
      else
        Error
          (Loc.exnf loc
             "Too many independant %s found:\n%s"
             M.desc
             (String.concat ~sep:"\n"
                (List.map (Set.to_list roots) ~f:(fun t ->
                   sprintf "- %S in %s"
                     (M.id t).name
                     (Path.to_string_maybe_quoted (M.id t).path)))))
end

type Lib.Sub_system.t +=
    Gen of (Library_compilation_context.t -> unit)

module Register_with_backend(M : With_backend) = struct
  include Jbuild.Sub_system_info.Register(M.Info)

  let gen info (c : Library_compilation_context.t) =
    let open Result.O in
    let backends =
      Lib.Compile.direct_requires c.compile_info >>= fun deps ->
      Lib.Compile.pps             c.compile_info >>= fun pps  ->
      M.Backend.select_backends
        ~loc:(M.Info.loc info)
        ~scope:c.scope
        ~written_by_user:(M.Info.backends info)
        (deps @ pps)
    in
    let fail, backends =
      match backends with
      | Ok backends -> (None, backends)
      | Error e ->
        (Some { fail = fun () -> raise e },
         [])
    in
    match fail with
    | None -> M.gen_rules c ~info ~backends
    | Some fail ->
      Super_context.prefix_rules c.super_context (Build.fail fail)
        ~f:(fun () -> M.gen_rules c ~info ~backends)

  include
    Lib.Sub_system.Register
      (struct
        module Info = M.Info
        type t = Library_compilation_context.t -> unit
        type Lib.Sub_system.t += T = Gen
        let instantiate ~resolve:_ ~get:_ _id info = gen info
        let to_sexp = None
      end)
end

let gen_rules (c : Library_compilation_context.t) =
  List.iter (Lib.Compile.sub_systems c.compile_info) ~f:(function
    | Gen gen -> gen c
    | _ -> ())
