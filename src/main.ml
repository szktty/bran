open Spotlib
open Base

exception Error

let limit = ref 1000

(*
let rec optimize n e =
  if n = 0 then
    e
  else
    let e' = Beta.f e in
      (*
    if e = e' then (* TODO: (=) operator raises Out_of_memory exception *)
      e
    else
       *)
      optimize (n - 1) e'
 *)

let modname path =
  let open Filepath in
  match dirbase & of_string os path with
  | _, None -> failwith "modname"
  | _, Some base -> fst & Xfilename.split_extension base

let modpath path =
  let open Filepath in
  match dirbase & of_string os path with
  | _, None -> failwith "modpath"
  | dir, Some base ->
    to_string & dir ^/ (fst & Xfilename.split_extension base) ^ ".erl"

let escript_path path =
  let open Filepath in
  match dirbase & of_string os path with
  | _, None -> failwith "modpath"
  | dir, Some base ->
    to_string & dir ^/ (fst & Xfilename.split_extension base)

let parse_test fpath =
  Log.verbose "# parsing test\n";
  let inchan = open_in fpath in
  let _ = Parser.prog Lexer.token (Lexing.from_channel inchan) in
  ()

(*
let parse l =
  Log.verbose "# parsing\n";
  Parser.prog Lexer.token l

let compile_ast outbuf t ~(fpath:string) =
  let mname = modname fpath in
  (*Log.debug "# %s\n" (Ast.to_string t);*)
  Log.verbose "# typing\n";
  let typing = Typing.f t in
  Log.verbose "# kNormal\n";
  let kNormal = KNormal.f typing in
  Log.verbose "# alpha\n";
  let alpha = Alpha.f kNormal in
(*
  Log.verbose "# optimization %d\n" !limit;
  let opt = optimize !limit alpha in
 *)
  Log.verbose "# closure\n";
  let _cl = Closure.f alpha in
   ()
    (*
  Log.verbose "# virtual\n";
  let v = Virtual.f cl in
  Log.verbose "# emit\n";
  Emit.f mname outbuf v;
  Context.finish_module ()
     *)

let print_error fpath loc =
  let start_line = Location.start_line loc + 1 in
  let end_line = Location.end_line loc + 1 in
  let start_col = Location.start_col loc + 1 in
  let end_col = Location.end_col loc in
  if start_line = end_line then
    Printf.printf "File \"%s\", line %d, characters %d-%d:\n"
      fpath start_line start_col end_col
  else
    Printf.printf "File \"%s\", between line and characters %d:%d-%d:%d:\n"
      fpath start_line end_line start_col end_col

let compile fpath = 
  Log.verbose "# compiling \"%s\"...\n" fpath;
  let inchan = open_in fpath in
  let err loc msg =
    close_in inchan;
    print_error fpath loc;
    Printf.printf "Error: %s\n" msg;
    raise Error
  in
  let outbuf = Buffer.create 128 in
  let mname = modname fpath in
  (* Context.init_module (String.capitalize mname); *)
  try
    let (t, last) = parse (Lexing.from_channel inchan) in
    if !Config.dry_run then
      exit 0;
    begin try
      compile_ast outbuf t ~fpath;
      close_in inchan;
      let outfpath =
        if !Config.escript then
          escript_path fpath
        else
          modpath fpath
      in
      let outchan = open_out outfpath in
      Buffer.output_buffer outchan outbuf;
      close_out outchan;
      if not !Config.compile_only then begin
        let open Spotlib.Xunix.Command in
        if !Config.escript then begin
          Unix.chmod outfpath 0o755
        end else begin
          let cmd_s = "erlc " ^ outfpath in
          Log.verbose "# $ %s\n" cmd_s;
          let cmd = shell cmd_s in
          begin match print ~prefix:"# erlc" cmd with
            | (Unix.WEXITED 0, _) -> Unix.unlink outfpath
            | _ -> Log.error "compilation failed"
          end
        end
      end
    with
    | Typing.Error (e, t1, t2) ->
      close_in inchan;
      print_error fpath e.loc;
      Log.error "type mismatch: actual %s, expected %s\n"
        (Type.to_string t2) (Type.to_string t1);
      raise Error
    | Typing.Top_level_error t ->
      close_in inchan;
      print_error fpath last.loc;
      Log.error "expression at the top level has type %s, expected unit\n"
        (Type.to_string t);
      raise Error
    | e -> raise e
    end
  with
    | Lexer.Error (loc, msg) -> err loc msg
    | Type.Parse_error (loc, msg) -> err loc msg
    | e -> raise e

let load_sig fpath = 
  Log.verbose "# loading signature file \"%s\"...\n" fpath;
  let inchan = open_in fpath in
  let err loc msg =
    close_in inchan;
    print_error fpath loc;
    Printf.printf "Error: %s\n" msg;
    raise Error
  in
  try
    let (t, _) = parse (Lexing.from_channel inchan) in
    close_in inchan
    (*Sig.load (String.capitalize & modname fpath) t*)
  with
    | Lexer.Error (loc, msg) -> err loc msg
    | Type.Parse_error (loc, msg) -> err loc msg
    (*| Sig.Error (loc, msg) -> err loc msg*)
    | e -> raise e
 *)

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
     ("-v", Arg.Unit (fun () -> Config.verbose := true), "print verbose messages");
     ("-V", Arg.Unit (fun () -> printf "%s\n" Version.version),
      "print version and exit");
    ]
    (fun s -> files := !files @ [s])
    (sprintf "Usage: %s [options] file" Sys.argv.(0));
  try
    List.iter
      (fun f ->
         (*Builtin.init ();*)
         match Xfilename.split_extension f with
         | (_, ".br") -> parse_test f (*ignore (compile f)*)
         | (_, ".bri") -> () (*ignore (load_sig f)*)
         | (_, ext) -> Log.error "unknown file extension - %s\n" ext)
      !files
  with
  | Error -> ()
  | e -> raise e
