module Outputs = struct
  type t =
    | Stdout
    | Stderr
    | Outputs (** Both Stdout and Stderr *)
end

module type Ast = sig
  type program
  type path
  type string

  type t =
    | Run            of program * string list
    | Chdir          of path * t
    | Setenv         of string * string * t
    | Redirect       of Outputs.t * path * t
    | Ignore         of Outputs.t * t
    | Progn          of t list
    | Echo           of string
    | Cat            of path
    | Copy           of path * path
    | Symlink        of path * path
    | Copy_and_add_line_directive of path * path
    | System         of string
    | Bash           of string
    | Write_file     of path * string
    | Rename         of path * path
    | Remove_tree    of path
    | Mkdir          of path
    | Digest_files   of path list
end

module type Helpers = sig
  include Ast

  val run : program -> string list -> t
  val chdir : path -> t -> t
  val setenv : string -> string -> t -> t
  val with_stdout_to : path -> t -> t
  val with_stderr_to : path -> t -> t
  val with_outputs_to : path -> t -> t
  val ignore_stdout : t -> t
  val ignore_stderr : t -> t
  val ignore_outputs : t -> t
  val progn : t list -> t
  val echo : string -> t
  val cat : path -> t
  val copy : path -> path -> t
  val symlink : path -> path -> t
  val copy_and_add_line_directive : path -> path -> t
  val system : string -> t
  val bash : string -> t
  val write_file : path -> string -> t
  val rename : path -> path -> t
  val remove_tree : path -> t
  val mkdir : path -> t
  val digest_files : path list -> t
end
