(* signature file *)

exception Error of Location.t * string
exception Pervasives_not_found

val find_lib_file : string -> string

val load : Id.t -> Ast_t.def list -> unit
val load_module : Id.t -> bool
val load_file : string -> bool

val create_env : unit -> Env.t
