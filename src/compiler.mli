(* frontend *)

val load_module : Id.t -> unit
val load_sig_file : string -> unit

val compile_file : string -> unit
val create_exec_file : string -> unit
