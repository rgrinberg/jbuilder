(** Build rules *)

open! Stdune
open! Import

(** {1 Setup} *)

(** {2 Creation} *)

module Error : sig
  (** Errors when building a target *)
  type t

  val info : t -> User_message.t * Path.t option

  val promotion : t -> Promotion.Annot.t option

  val id : t -> int
end

(** The current set of active errors *)
val errors : unit -> Error.t list

module Context_or_install : sig
  type t =
    | Install of Context_name.t
    | Context of Context_name.t

  val to_dyn : t -> Dyn.t
end

module Subdir_set : sig
  type t =
    | All
    | These of String.Set.t

  val empty : t

  val union : t -> t -> t

  val union_all : t list -> t

  val mem : t -> string -> bool
end

type extra_sub_directories_to_keep = Subdir_set.t

module type Rule_generator = sig
  (** The rule generator.

      This callback is used to generate the rules for a given directory in the
      corresponding build context. It receives the directory for which to
      generate the rules and the split part of the path after the build context.
      It must return an additional list of sub-directories to keep. This is in
      addition to the ones that are present in the source tree and the ones that
      already contain rules.

      It is expected that [gen_rules] only generate rules whose targets are
      descendant of [dir].

      The callback should return [None] if it doesn't know about the given
      [Context_or_install.t]. *)
  val gen_rules :
       Context_or_install.t
    -> dir:Path.Build.t
    -> string list
    -> (extra_sub_directories_to_keep * Rules.t) option Memo.Build.t

  (** [global_rules] is a way to generate rules in arbitrary directories
      upfront. *)
  val global_rules : Rules.t Memo.Lazy.t
end

module Handler : sig
  (** Callbacks for build related events *)
  type t

  type event =
    | Start  (** New build started *)
    | Finish  (** Build finished successfully *)

  type error =
    | Add of Error.t  (** Error encountered while building *)
    | Remove of Error.t  (** An existing error is invalidated *)

  val create :
       error:(error list -> unit Fiber.t) (** Callback for build [error] *)
    -> build_progress:(complete:int -> remaining:int -> unit Fiber.t)
         (** Callback whenever there's build progress to report *)
    -> build_event:(event -> unit Fiber.t) (** Called for every [event] *)
    -> t
end

(** Initializes the build system. This must be called first. *)
val init :
     stats:Dune_stats.t option
  -> contexts:Build_context.t list Memo.Lazy.t
  -> promote_source:
       (   ?chmod:(int -> int)
        -> src:Path.Build.t
        -> dst:Path.Source.t
        -> Build_context.t option
        -> unit Fiber.t)
  -> cache_config:Dune_cache.Config.t
  -> cache_debug_flags:Cache_debug_flags.t
  -> sandboxing_preference:Sandbox_mode.t list
  -> rule_generator:(module Rule_generator)
  -> handler:Handler.t option
  -> unit

(** {2 Primitive for rule generations} *)

(** [prefix_rules prefix ~f] Runs [f] and adds [prefix] as a dependency to all
    the rules generated by [f] *)
val prefix_rules :
  unit Action_builder.t -> f:(unit -> 'a Memo.Build.t) -> 'a Memo.Build.t

(** [eval_pred glob] returns the list of files in [File_selector.dir glob] that
    matches [File_selector.predicate glob]. The list of files includes the list
    of targets. *)
val eval_pred : File_selector.t -> Path.Set.t Memo.Build.t

(** Returns the set of targets in the given directory. *)
val targets_of : dir:Path.t -> Path.Set.t Memo.Build.t

(** Load the rules for this directory. *)
val load_dir : dir:Path.t -> unit Memo.Build.t

(** Assuming [files] is the list of files in [_build/install] that belong to
    package [pkg], [package_deps t pkg files] is the set of direct package
    dependencies of [package]. *)
val package_deps :
     packages_of:(Path.Build.t -> Package.Id.Set.t Memo.Build.t)
  -> Package.t
  -> Path.Set.t
  -> Package.Id.Set.t Memo.Build.t

(** {2 Aliases} *)

module Alias : sig
  type t = Alias.t

  (** Alias for all the files in [_build/install] that belong to this package *)
  val package_install : context:Build_context.t -> pkg:Package.t -> t

  (** Depend on the expansion of this alias. *)
  val dep : t -> unit Action_builder.t

  (** Implements [@@alias] on the command line *)
  val dep_multi_contexts :
       dir:Path.Source.t
    -> name:Alias.Name.t
    -> contexts:Context_name.t list
    -> unit Action_builder.t

  (** Implements [(alias_rec ...)] in dependency specification *)
  val dep_rec : t -> loc:Loc.t -> unit Action_builder.t

  (** Implements [@alias] on the command line *)
  val dep_rec_multi_contexts :
       dir:Path.Source.t
    -> name:Alias.Name.t
    -> contexts:Context_name.t list
    -> unit Action_builder.t
end

(** {1 Requests} *)

(** Build a request *)
val build : 'a Action_builder.t -> 'a Memo.Build.t

val is_target : Path.t -> bool Memo.Build.t

(** List of all buildable targets. *)
val all_targets : unit -> Path.Build.Set.t Memo.Build.t

(** The set of files that were created in the source tree and need to be
    deleted. *)
val files_in_source_tree_to_delete : unit -> Path.Set.t

(** {2 Build rules} *)

module For_command_line : sig
  (** Functions in this module duplicate some work that is done by [build] and
      other functions, so they not suitable to be called as part of a normal
      build. However, we need them in some part of the command line. *)

  (** A fully evaluated rule. *)
  module Rule : sig
    type t = private
      { id : Rule.Id.t
      ; dir : Path.Build.t
      ; deps : Dep.Set.t
      ; expanded_deps : Path.Set.t
      ; targets : Path.Build.Set.t
      ; context : Build_context.t option
      ; action : Action.t
      }
  end

  (** Return the list of fully evaluated rules used to build the given targets.
      If [recursive] is [true], also include the rules needed to build the
      transitive dependencies of the targets. *)
  val evaluate_rules :
    recursive:bool -> request:unit Action_builder.t -> Rule.t list Memo.Build.t

  (** Similar to [build], but doesn't build the dependencies, only expand them *)
  val eval_build_request : 'a Action_builder.t -> ('a * Dep.Set.t) Memo.Build.t
end

(** {2 Running a build} *)

val run :
     ?report_error:(Exn_with_backtrace.t -> unit)
  -> (unit -> 'a Memo.Build.t)
  -> 'a Fiber.t

(** {2 Misc} *)

module Progress : sig
  type t =
    { number_of_rules_discovered : int
    ; number_of_rules_executed : int
    }
end

val get_current_progress : unit -> Progress.t
