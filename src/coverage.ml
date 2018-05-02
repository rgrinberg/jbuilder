open! Import

include Coverage0

module Eval_dirs = Ordered_set_lang.Make(Path)(struct
    type t = Path.t
    type key = Path.t
    let key p = p
  end)

module Eval_modules = Ordered_set_lang.Make(Module.Name)(struct
    type t = Module.t
    type key = Module.Name.t
    let key m = Module.name m
  end)

let lib_covered (t : Coverage0.t) ~lib_dir =
  let covered_dirs =
    Eval_dirs.eval_unordered ~parse:(fun ~loc:_ s -> Path.of_string s)
      ~standard:(Path.Map.singleton Path.root Path.root) t.covered_dirs in
  Path.Map.exists covered_dirs ~f:(Path.is_descendant ~of_:lib_dir)

let apply_for_compile_info (t : Coverage0.t) ~(lib : Jbuild.Library.t)
      ~lib_dir =
  if lib_covered t ~lib_dir then (
    let pp_coverage =
      { Jbuild.Preprocess.
        pps = [Loc.none, Jbuild.Pp.of_string t.coverage]
      ; flags = []
      } in
    let instrumented = ref false in
    let lib =
      { lib with
        buildable =
          { lib.buildable
            with preprocess = (
              Jbuild.Per_module.map ~f:(fun pp ->
                match (pp : Jbuild.Preprocess.t) with
                | No_preprocessing ->
                  instrumented := true;
                  Jbuild.Preprocess.Pps pp_coverage
                | Pps pp ->
                  instrumented := true;
                  Pps { pps = pp_coverage.pps @ pp.pps
                      ; flags = pp_coverage.flags @ pp.flags }
                | Action _ -> pp
              ) lib.buildable.preprocess
            )
          }
      } in
    if !instrumented then
      lib
    else
      die "Unable to add instrumentation to library %S" lib.name
  ) else (
    lib
  )

(* let apply_instrumented
 *       (t : Coverage0.t)
 *       ~(lib : Jbuild.Library.t)
 *       ~lib_dir
 *       ~(modules : Module.t Module.Name.Map.t) =
 *   if lib_covered t ~lib_dir then (
 *     let covered_modules =
 *       Eval_modules.eval_unordered
 *         lib.bisect.modules
 *         ~parse:(fun ~loc:_ s ->
 *           (\* TODO proper handling *\)
 *           Option.value_exn (
 *             Module.Name.Map.find modules (Module.Name.of_string s)
 *           ))
 *         ~standard:modules
 *     in
 *     let covered_pp : Jbuild.Preprocess.pps option Jbuild.Per_module.t =
 *       let pps =
 *         Some { Jbuild.Preprocess.
 *                pps = [Loc.none, Jbuild.Pp.of_string t.coverage]
 *              ; flags = []
 *              } in
 *       Jbuild.Per_module.of_mapping
 *         ~default:None
 *         (covered_modules
 *         |> Module.Name.Map.keys
 *         |> List.map ~f:(fun m -> ([m], pps)))
 *       |> function
 *       | Ok x -> x
 *       | Error _ -> failwith "TODO"
 *     in
 *     let combined_pp =
 *       Jbuild.Per_module.merge lib.buildable.preprocess covered_pp
 *         ~f:(fun _m lib_pp covered_pp ->
 *           match lib_pp, covered_pp with
 *           | pp, None -> pp
 *           | No_preprocessing, Some pp -> Jbuild.Preprocess.Pps pp
 *           | Action _, Some _ ->
 *             Loc.fail lib.buildable.loc "Unable to add bisect pp"
 *           | Pps pp, Some cpps ->
 *             Pps { pps = cpps.pps @ pp.pps
 *                 ; flags = cpps.flags @ pp.flags })
 *         ~default:Jbuild.Preprocess.No_preprocessing
 *     in
 *     { lib with
 *       buildable = {lib.buildable with preprocess = combined_pp}
 *     }
 *   ) else (
 *     lib
 *   ) *)

let preprocess coverage ~(bisect : Jbuild.Bisect.t) ~modules ~preprocess =
  let covered_modules =
    Eval_modules.eval_unordered bisect.modules
      ~parse:(fun ~loc:_ s ->
        Option.value_exn (
          Module.Name.Map.find modules (Module.Name.of_string s)
        )
      )
      ~standard:modules
  in
  ignore (coverage, covered_modules);
  preprocess
