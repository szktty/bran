let debug = ref false
let verbose = ref false

let syntax_only = ref false
let compile_only = ref false
let erl_opts = ref None
let escript = ref false
let emu_args = ref None
let load_paths = ref []
let add_load_path path = load_paths := !load_paths @ [path]
let gen_sig_file = ref false

let get_env_libs () =
  try Some (Sys.getenv "BRAN_LIBS")
  with Not_found -> None

let print_tycon = ref None
let print_type = ref None
