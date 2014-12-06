open Spotlib.Base
open X

let bprint_text_of_loc oc fpath loc indent =
  let open Printf in
  let open Location in
  let rec back ic =
    match pos_in ic with
    | 0 -> ()
    | orig ->
      match input_char ic with
      | '\r' | '\n' -> ()
      | c ->
        seek_in ic (orig - 1);
        back ic
  in
  with_ic (open_in fpath)
    (fun ic ->
       seek_in ic (start_offset loc);
       back ic;
       let text = input_line ic in
       bprintf oc "%s%s\n" (String.make indent ' ') text;
       bprintf oc "%s%s\n" (String.make (indent + start_col loc) ' ')
         (String.make (end_offset loc - start_offset loc) '^');
       ())

let print_error fpath loc msg =
  let start_line = Location.start_line loc + 1 in
  let end_line = Location.end_line loc + 1 in
  let start_col = Location.start_col loc + 1 in
  let end_col = Location.end_col loc + 1 in
  if start_line = end_line then begin
    if start_col = end_col then
      Printf.printf "File \"%s\", line %d, characters %d:\n"
        fpath start_line start_col
    else
      Printf.printf "File \"%s\", line %d, characters %d-%d:\n"
        fpath start_line start_col end_col
    end
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
         match Filename.split_extension fpath with
         | (_, ".br") -> ignore & Compiler.compile_file fpath
         | (_, ".bri") -> ignore & Sig.load_file fpath
         | (_, ext) -> Log.error "Unknown file extension %s\n" ext
       with
       | Lexer.Error (loc, msg) -> print_error fpath loc msg
       | Sig.Error (loc, msg) -> print_error fpath loc msg
       | Ast_t.Syntax_error (loc, None) ->
         print_error fpath loc "Syntax error"
       | Ast_t.Syntax_error (loc, Some msg) ->
         print_error fpath loc ("Syntax error: " ^ msg)
       | Ast_t.Unbound_value_error (loc, x) ->
         print_error fpath loc ("Unbound value `" ^ x ^ "'")
       | Ast_t.Unbound_module_error (loc, x) ->
         print_error fpath loc ("Unbound module `" ^ x ^ "'")
       | Typing.Error (e, t1, t2) ->
         let print_type oc t =
           let (sl1, sc1, el1, ec1) = Location.values1 t.Locating.loc in
           let lc = sprintf "(%d:%d-%d:%d)" sl1 sc1 el1 ec1 in
           let name = Type.name t in
           bprintf oc "    %s    %s"
             (lc ^ (String.make (String.length lc mod 4) ' '))
             (name ^ (String.make (String.length name mod 4) ' '))
         in
         let buf = Buffer.create 16 in
         bprintf buf "Type mismatch: This expression has type `%s', but the expression was expected of type `%s'\n"
           (Type.name t2) (Type.name t1);
         print_type buf t2;
         bprintf buf "    <- actual type\n\n";
         bprint_text_of_loc buf fpath t2.loc 4;
         bprintf buf "\n";
         print_type buf t1;
         bprintf buf "    <- expected type\n\n";
          bprint_text_of_loc buf fpath t1.loc 4;
         print_error fpath e.loc (Buffer.contents buf)
       | e -> raise e)
    !files
