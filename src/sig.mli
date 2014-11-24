(* signature file *)

exception Error of Location.t * string

val find_lib_file : string -> string

val load : Id.t -> Syntax.def list -> unit
