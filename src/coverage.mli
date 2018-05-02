open Stdune

val apply_for_compile_info
  :  Coverage0.t
  -> lib:Jbuild.Library.t
  -> lib_dir:Path.t
  -> Jbuild.Library.t

(* val apply_instrumented
 *   :  Coverage0.t
 *   -> lib:Jbuild.Library.t
 *   -> lib_dir:Path.t
 *   -> modules:Module.t Module.Name.Map.t
 *   -> Jbuild.Library.t *)

val preprocess
  :  Coverage0.t
  -> bisect:Jbuild.Bisect.t
  -> modules:Module.t Module.Name.Map.t
  -> preprocess:Jbuild.Preprocess_map.t
  -> Jbuild.Preprocess_map.t
