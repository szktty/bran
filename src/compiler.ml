open Spotlib.Base

exception Error of string

let compile_erl_file fpath =
  let open Spotlib.Xunix.Command in
  let (dir, _) = Utils.dirbase fpath in
  let dir' = Spotlib.Filepath.to_string dir in
  let cmd_s = Printf.sprintf "erlc -o %s %s" dir' fpath in
  Log.verbose "# $ %s\n" cmd_s;
  let cmd = shell cmd_s in
  match print ~prefix:"# erlc" cmd with
  | (Unix.WEXITED 0, _) -> ()
  | _ -> raise (Error "Erlang compilation failed")

let create_exec_file fpath =
  let open Printf in
  let buf = Buffer.create 1 in
  let (exec, _) = Spotlib.Xfilename.split_extension fpath in
  bprintf buf "erl -boot start_clean -noinput -s init stop -eval '";
  bprintf buf "{ok, _, Beam} = compile:file(\"%s\", [binary, compressed, debug_info]), " fpath;
  bprintf buf "escript:create(\"%s\", [shebang, {beam, Beam}, " exec;
  bprintf buf "{emu_args, \"-pa bran/ebin %s\"}])'"
    (Spotlib.Option.default !Config.emu_args (fun () -> ""));
  let cmd_s = Buffer.contents buf in
  Log.verbose "# $ %s\n" cmd_s;
  let cmd = Spotlib.Xunix.Command.shell cmd_s in
  match Spotlib.Xunix.Command.print ~prefix:"# erl" cmd with
  | (Unix.WEXITED 0, _) -> ()
  | _ -> raise (Error "Executable file creation failed")

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

let gen_sig_file fpath defs =
  let open Syntax in
  Log.verbose "# generate signature file\n";
  let lines = fold (fun (env, accu) def ->
      match def.desc with
      | TypeDef (x, t) ->
        (* FIXME *)
        Printf.sprintf "type %s = %s" x (Type.ocaml_of_tycon t) :: accu
      | VarDef ((x, t), _) ->
        Printf.sprintf "var %s : %s" x (Type.ocaml_of t) :: accu
      | RecDef { name = (x, t) } ->
        Printf.sprintf "def %s : %s" x (Type.ocaml_of t) :: accu
      | _ -> accu)
      defs (Sig.create_env ())
  in
  let oc = open_out & Utils.replace_ext fpath ".auto.bri" in
  let s = String.concat "\n" lines in
  Log.verbose "%s\n" s;
  Printf.fprintf oc "%s\n" s

let compile_file fpath =
  let typed = Typing.f & Utils.parse_file fpath in
  let prog = Erlang.f & Closure.f & Alpha.f & KNormal.f typed in
  let mname = Utils.module_name fpath in
  let outbuf = Buffer.create 128 in
  Emit.f mname outbuf prog;
  let outfpath = Utils.erl_path fpath in
  let outchan = open_out outfpath in
  Buffer.output_buffer outchan outbuf;
  close_out outchan;

  if !Config.gen_sig_file then
    gen_sig_file fpath typed;

  if not !Config.compile_only then begin
    if !Config.escript then
      create_exec_file outfpath
    else
      compile_erl_file outfpath;
    Unix.unlink outfpath
  end
