open Spotlib.Base
open X

(* entry point *)
let () =
  let open Printf in
  let files = ref [] in
  Arg.parse
    [("-c", Arg.Unit (fun () -> Config.compile_only := true),
      "compile only, output Erlang source");
     ("-d", Arg.Unit (fun () -> Config.debug := true), "print debug messages");
     ("-emu-args", Arg.String (fun v -> Config.emu_args := Some v),
      "Erlang emulator flag embedded into executable file");
     ("-erl", Arg.String (fun v -> Config.erl_opts := Some v),
      "Erlang compiler (erlc) options");
     ("-escript", Arg.Unit (fun () -> Config.escript := true),
      "create an executable file");
     ("-i", Arg.Unit (fun () -> Config.gen_sig_file := true),
      "generate inferred interface to signature file (*.auto.bri)");
     ("-I", Arg.String Config.add_load_path, "add the path to load path list");
     ("-s", Arg.Unit (fun () -> Config.syntax_only := true), "check syntax only");
     ("-v", Arg.Unit (fun () -> Config.verbose := true), "print verbose messages");
     ("-V", Arg.Unit (fun () -> printf "%s\n" Version.version),
      "print version and exit");
    ]
    (fun s -> files := !files @ [s])
    (sprintf "Usage: %s [options] file" Sys.argv.(0));
  if List.length !files = 0 then begin
    Printf.printf "Error: No input files\n";
    Printf.printf "Try `-help' option for usage information.\n";
    exit 1
  end;

  (* getenv BRAN_LIBS *)
  Spotlib.Option.iter
    (fun path ->
       Log.verbose "# BRAN_LIBS = %s\n" path;
       Config.add_load_path path) & Config.get_env_libs ();

  List.iter
    (fun fpath ->
       try
         match Filename.split_extension fpath with
         | (_, ".br") -> ignore & Compiler.compile_file fpath
         | (_, ".bri") -> ignore & Sig.load_file fpath
         | (_, ext) -> Log.error "Unknown file extension %s\n" ext
       with
       | e -> Console.print_exc fpath e)
    !files
