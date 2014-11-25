(* command-line application testing *)

module FileChange : sig

  type change =
    | Not_changed
    | Accessed
    | Created
    | Modified
    | Changed
    | Deleted

  type t = {
    path : string;
    change : change;
    time : float;
  }

  val path : t -> string
  val change : t -> change
  val time : t -> float

  val to_string : t -> string
  val change_to_string : change -> string

end

module Result : sig

  type t = {
    stdout : string;
    stderr : string;
    status : Unix.process_status;
    file_changes : FileChange.t list;
  }

  val files_not_changed : t -> string list
  val files_created : t -> string list
  val files_accessed : t -> string list
  val files_modified : t -> string list
  val files_changed : t -> string list
  val files_deleted : t -> string list
  val files_updated : t -> FileChange.t list
  val has_file_changes : t -> bool
  val has_files_created_only : t -> string list -> bool

  val return_code : t -> int option
  val is_exited : t -> bool
  val is_succeded : t -> bool

end

module Env : sig

  type t

  val create :
    ?env:((string * string) list)
    -> ?chdir:string
    -> ?start_clear:bool
    -> ?ignore_files:string list
    -> ?ignore_hidden:bool
    -> string
    -> t
  (** Create an environment at the base path *)

  val clear : ?force:bool -> t -> unit
  (** Delete all the files in the base directory *)

  val run :
    ?expect_error:bool
    -> ?expect_stderr:bool
    -> ?chdir:string
    -> ?quiet:bool
    -> t
    -> f:(t -> string list)
    -> Result.t

  val install : t -> string -> string
  (** Copy the file to the base directory *)

  val write : t -> string -> (out_channel -> unit) -> unit

end

val run :
  ?env:((string * string) list)
  -> ?chdir:string
  -> ?start_clear:bool
  -> ?ignore_files:string list
  -> ?ignore_hidden:bool
  -> ?expect_error:bool
  -> ?expect_stderr:bool
  -> ?chdir:string
  -> ?quiet:bool
  -> ?basedir:string
  -> (Env.t -> string list)
  -> Result.t

val replace_extension : string -> string -> string
(** [replace_extension path extension]
  * replace extension of [path] with [extension].
  * [replace_extension "hello.world.txt" ".bin" = "hello.world.bin"]
  * [replace_extension "hello_world" ".txt" = "hello_world.txt"]
  *)
