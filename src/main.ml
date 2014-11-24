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
     ("-dry-run", Arg.Unit (fun () -> Config.dry_run := true), "parse syntax only");
     ("-erl", Arg.String (fun v -> Config.erl_opts := Some v),
      "Erlang compiler (erlc) options");
     ("-escript", Arg.Unit (fun () -> Config.escript := true),
      "create an executable file");
     ("-I", Arg.String (fun v ->
                          Config.load_paths := !Config.load_paths @ [v]),
      "add the path to load path list");
     ("-v", Arg.Unit (fun () -> Config.verbose := true), "print verbose messages");
     ("-V", Arg.Unit (fun () -> printf "%s\n" Version.version),
      "print version and exit");
    ]
    (fun s -> files := !files @ [s])
    (sprintf "Usage: %s [options] file" Sys.argv.(0));

  List.iter
    (fun fpath ->
       try
         match Spotlib.Xfilename.split_extension fpath with
         | (_, ".br") -> ignore & Compiler.compile_file fpath
         | (_, ".bri") -> ignore & Compiler.load_sig_file fpath
         | (_, ext) -> Log.error "Unknown file extension %s\n" ext
       with
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
