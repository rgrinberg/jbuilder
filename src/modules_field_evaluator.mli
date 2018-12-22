open! Stdune

type t = private
  { all_modules : Module.Name_map.t
  ; virtual_modules : Module.Name_map.t
  }

val eval
  :  modules:(Module.Source.t Module.Name.Map.t)
  -> obj_dir:Module.Obj_dir.t
  -> buildable:Dune_file.Buildable.t
  -> virtual_modules:Ordered_set_lang.t option
  -> private_modules:Ordered_set_lang.t
  -> t
