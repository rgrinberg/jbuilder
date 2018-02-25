val colorize : key:string -> string -> string

val stderr_supports_colors : bool Lazy.t
val setup_env_for_colors : unit Lazy.t

(** Strip colors in [not (Lazy.force stderr_supports_colors)] *)
val strip_colors_for_stderr : string -> string

(** Enable the interpretation of color tags for [Format.err_formatter] *)
val setup_err_formatter_colors : unit -> unit

type styles

val output_filename : styles

val apply_string : styles -> string -> string
