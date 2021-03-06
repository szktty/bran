(* command-line application testing *)

val debug : bool ref

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
    predictions : FileChange.t list;
  }

  val changes : t -> FileChange.t list
  val find_files_changed : t -> FileChange.change -> string list
  val files_updated : t -> FileChange.t list
  val has_file_changes : t -> bool
  val has_files_created_only : t -> string list -> bool

  val return_code : t -> int option
  val is_exited : t -> bool
  val is_succeeded : t -> bool

  val prediction : t -> [`Success | `Failure]

end

module Env : sig

  type t

  val create :
    ?env:((string * string) list)
    -> ?start_clear:bool
    -> ?ignore_files:string list
    -> ?ignore_hidden:bool
    -> ?parallel:bool
    -> ?expect_error:bool
    -> ?expect_stderr:bool
    -> ?quiet:bool
    -> ?basedir:string
    -> unit
    -> t
  (** Create an environment at the base path *)

  val clear : ?force:bool -> t -> unit
  (** Delete all the files in the base directory *)

  val run : ?chdir:string -> t -> (t -> 'a) -> 'a

  val shell : t -> string list -> Result.t

  val install : t -> string -> string
  (** Copy the file to the base directory *)

  val write : t -> string -> (out_channel -> unit) -> unit

  val predict : t -> string -> FileChange.change -> unit
  (** Register the file name will be changed at running.
   *  The result of prediction can be confirmed with Result.prediction *)

end

val run :
  ?env:((string * string) list)
  -> ?start_clear:bool
  -> ?ignore_files:string list
  -> ?ignore_hidden:bool
  -> ?parallel:bool
  -> ?expect_error:bool
  -> ?expect_stderr:bool
  -> ?quiet:bool
  -> ?basedir:string
  -> (Env.t -> 'a)
  -> 'a

val replace_extension : string -> string -> string
(** [replace_extension path extension]
  * replace extension of [path] with [extension].
  * [replace_extension "hello.world.txt" ".bin" = "hello.world.bin"]
  * [replace_extension "hello_world" ".txt" = "hello_world.txt"]
  *)
