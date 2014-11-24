val filepath_of_string : string -> Spotlib.Filepath.t
val dirbase : string -> Spotlib.Filepath.t * string option
val module_name : string -> string
val erl_path : string -> string
val escript_path : string -> string
val parse_file : string -> Syntax.def list
