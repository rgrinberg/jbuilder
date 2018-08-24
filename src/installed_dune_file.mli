(** Dune files that are installed on the system *)

open! Stdune

module Virtual_library : sig
  type t =
    { virtual_modules: Module.t Module.Name.Map.t
    }
end

type t =
  { virtual_library : Virtual_library.t option
  ; sub_systems     : Dune_file.Sub_system_info.t Sub_system_name.Map.t
  }

val empty : t

val load : Path.t -> t
val gen
  : dune_version:Syntax.Version.t
  -> virtual_library:Virtual_library.t option
  -> (Syntax.Version.t * Dsexp.t) Sub_system_name.Map.t
  -> Dsexp.t
