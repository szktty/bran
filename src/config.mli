val debug : bool ref
val verbose : bool ref

val syntax_only : bool ref
val compile_only : bool ref
val erl_opts : string option ref
val escript : bool ref
val emu_args : string option ref
val load_paths : string list ref
val add_load_path : string -> unit
val gen_sig_file : bool ref

val get_env_libs : unit -> string option

(* debug *)
val print_tycon : string option ref
val print_type : string option ref
