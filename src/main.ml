open Base

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
     ("-spec", Arg.Unit (fun () -> Config.gen_spec := true),
      "generate -spec for functions");
     ("-v", Arg.Unit (fun () -> Config.verbose := true), "print verbose messages");
     ("-V", Arg.Unit (fun () -> printf "%s\n" Version.version),
      "print version and exit");
     ("-print-tycon", Arg.String (fun v -> Config.print_tycon := Some v),
      "print type constructor of type (for debug)");
     ("-print-type", Arg.String (fun v -> Config.print_type := Some v),
      "print type of value (for debug)");
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
         begin
           begin match Filename.split_extension fpath with
           | (_, ".br") -> ignore & Compiler.compile_file fpath
           | (_, ".bri") -> ignore & Sig.load_file fpath
           | (_, ext) -> Log.error "Unknown file extension %s\n" ext
           end;

           let print_type f p name =
             let binding name =
               Binding.of_string &
               match String.index_opt name '.' with
               | None -> (Utils.module_name fpath) ^ "." ^ name
               | Some _ -> name
             in
             match name with
             | None -> ()
             | Some name ->
               try begin
                 match f & binding name with
                 | None -> Spotlib.Exn.failwithf "Value `%s' is not found" name
                 | Some t -> Printf.printf "%s" (p t)
               end with
               | Binding.Invalid_path -> failwith "Invalid binding path"
           in

           (* debug: -print-tycon *)
           print_type Library.find_tycon_opt Type.Tycon.to_repr !Config.print_tycon;

           (* debug: -print-type *)
           print_type Library.find_val_opt Type.to_repr !Config.print_type
         end
       with
       | e -> Console.print_exc fpath e)
    !files
