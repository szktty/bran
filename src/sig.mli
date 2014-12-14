(* signature file *)

exception Error of Location.t * string
exception Pervasives_not_found

val find_lib_file : string -> string

val load : Source.t -> unit
val load_module : Binding.t -> [`Ok | `Error]
val load_file : string -> [`Ok | `Error]

val create_env : unit -> Env.t
