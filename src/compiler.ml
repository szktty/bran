open Spotlib.Base

let filepath_of_string path = 
  let open Spotlib.Filepath in
  of_string os path

let dirbase path =
  Spotlib.Filepath.dirbase & filepath_of_string path

let module_name path =
  match dirbase path with
  | _, None -> failwith "mod_name"
  | _, Some base -> fst & Spotlib.Xfilename.split_extension base

let erl_path path =
  match dirbase path with
  | _, None -> failwith "erl_path"
  | dir, Some base ->
    let open Spotlib.Filepath in
    to_string & dir ^/ (fst & Spotlib.Xfilename.split_extension base) ^ ".erl"

let escript_path path =
  let open Spotlib.Filepath in
  match dirbase & of_string os path with
  | _, None -> failwith "modpath"
  | dir, Some base ->
    to_string & dir ^/ (fst & Spotlib.Xfilename.split_extension base)

let parse_file fpath =
  Log.verbose "# Parsing\n";
  let inchan = open_in fpath in
  let prog = Parser.prog Lexer.token (Lexing.from_channel inchan) in
  Log.debug "# %s\n" (String.concat ";\n " (List.map Syntax.string_of_def prog));
  prog

let load_sig_file fpath = 
  Log.verbose "# loading signature file \"%s\"...\n" fpath;
  let fpath' = Sig.find_lib_file fpath in
  Log.verbose "#    load %s\n" fpath';
  Sig.load (String.capitalize & module_name fpath) & parse_file fpath'

let load_module name =
  load_sig_file & String.uncapitalize name ^ ".bri"

let compile_erl_file fpath =
  let open Spotlib.Xunix.Command in
  let cmd_s = "erlc " ^ fpath in
  Log.verbose "# $ %s\n" cmd_s;
  let cmd = shell cmd_s in
  match print ~prefix:"# erlc" cmd with
  | (Unix.WEXITED 0, _) -> Unix.unlink fpath
  | _ -> Log.error "compilation failed"

let create_exec_file fpath =
  Unix.chmod fpath 0o755

(*
let limit = ref 1000
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

let compile_file fpath =
  let prog = Erlang.f & Closure.f & Alpha.f & KNormal.f &
             Typing.f & parse_file fpath in
  let mname = module_name fpath in
  let outbuf = Buffer.create 128 in
  Emit.f mname outbuf prog;
  let outfpath =
    if !Config.escript then
      escript_path fpath
    else
      erl_path fpath
  in
  let outchan = open_out outfpath in
  Buffer.output_buffer outchan outbuf;
  close_out outchan;

  if not !Config.compile_only then begin
    compile_erl_file outfpath;
    if !Config.escript then
      create_exec_file outfpath
  end
