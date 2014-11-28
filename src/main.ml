open Spotlib.Base

let print_error fpath loc msg =
  let start_line = Location.start_line loc + 1 in
  let end_line = Location.end_line loc + 1 in
  let start_col = Location.start_col loc + 1 in
  let end_col = Location.end_col loc in
  if start_line = end_line then
    Printf.printf "File \"%s\", line %d, characters %d-%d:\n"
      fpath start_line start_col end_col
  else
    Printf.printf "File \"%s\", between line and characters %d:%d-%d:%d:\n"
      fpath start_line start_col end_line end_col;
  Log.error "%s\n" msg;
  exit 1

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
         match Spotlib.Xfilename.split_extension fpath with
         | (_, ".br") -> ignore & Compiler.compile_file fpath
         | (_, ".bri") -> ignore & Sig.load_file fpath
         | (_, ext) -> Log.error "Unknown file extension %s\n" ext
       with
       | Lexer.Error (loc, msg) -> print_error fpath loc msg
       | Sig.Error (loc, msg) -> print_error fpath loc msg
       | Syntax.Syntax_error loc -> print_error fpath loc "Syntax error"
       | Syntax.Unbound_value_error (loc, x) ->
         print_error fpath loc ("Unbound value `" ^ x ^ "'")
       | Syntax.Unbound_module_error (loc, x) ->
         print_error fpath loc ("Unbound module `" ^ x ^ "'")
       | Typing.Error (e, t1, t2) ->
         print_error fpath e.loc
           (Printf.sprintf "Type mismatch: actual %s, expected %s"
              (Type.name t2) (Type.name t1))
       | e -> raise e)
    !files
