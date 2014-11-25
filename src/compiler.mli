(* frontend *)

exception Error of string

val compile_file : string -> unit
val create_exec_file : string -> unit
