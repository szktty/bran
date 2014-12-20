open Base

exception Error of string

let compile_erl_file fpath =
  let open Unix.Command in
  let (dir, _) = Utils.dirbase fpath in
  let dir' = Spotlib.Filepath.to_string dir in
  let cmd_s = Printf.sprintf "erlc -W0 -o %s %s" dir' fpath in
  Log.verbose "# $ %s\n" cmd_s;
  let cmd = shell cmd_s in
  match print ~prefix:"# erlc" cmd with
  | (Unix.WEXITED 0, _) -> ()
  | _ -> raise (Error "Erlang compilation failed")

let create_exec_file fpath =
  let open Printf in
  let buf = Buffer.create 1 in
  let (exec, _) = Filename.split_extension fpath in
  bprintf buf "erl -boot start_clean -noinput -s init stop -eval '";
  bprintf buf "{ok, _, Beam} = compile:file(\"%s\", [binary, compressed, debug_info]), " fpath;
  bprintf buf "escript:create(\"%s\", [shebang, {beam, Beam}, " exec;
  bprintf buf "{emu_args, \"-pa bran/ebin %s\"}])'"
    (Spotlib.Option.default !Config.emu_args (fun () -> ""));
  let cmd_s = Buffer.contents buf in
  Log.verbose "# $ %s\n" cmd_s;
  let cmd = Unix.Command.shell cmd_s in
  match Unix.Command.print ~prefix:"# erl" cmd with
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
  let open Ast_t in
  Log.verbose "# generate signature file\n";
  let lines = Ast.fold (fun (env, accu) def ->
      match def.desc with
      | TypeDef (x, t) ->
        (* FIXME *)
        Printf.sprintf "type %s = %s" x (Type.Tycon.to_repr t) :: accu
      | VarDef ((x, t), _) ->
        Printf.sprintf "var %s : %s" x (Type.to_repr t) :: accu
      | RecDef { name = (x, t) } ->
        Printf.sprintf "def %s : %s" x (Type.to_repr t) :: accu
      | _ -> accu)
      defs (Sig.create_env ())
  in
  let oc = open_out & Utils.replace_ext fpath ".auto.bri" in
  let s = String.concat "\n" lines in
  Log.verbose "%s\n" s;
  Printf.fprintf oc "%s\n" s

let parse defs =
  let open Ast_t in
  let open Location.With in
  let parse' (tycons, vals, exts) def =
    match def.desc with
    | TypeDef (x, tycon) ->
      Log.debug "# type %s : %s\n" x (Type.Tycon.to_string tycon);
      ((x, tycon) :: tycons, vals, exts)
    | VarDef ((x, t), _) ->
      Log.debug "# var %s : %s\n" x (Type.to_string t);
      (tycons, (x, t) :: vals, exts)
    | RecDef { name = (x, t) } ->
      Log.debug "# def %s : %s\n" x (Type.to_string t);
      (tycons, (x, t) :: vals, exts)
    | SigDef _ ->
      raise (Sig.Error (def.with_, "Signature definition only at .bri file"))
    | _ -> (tycons, vals, exts)
  in
  List.fold_left parse' ([], [], []) defs

let register name defs =
  Log.verbose "# register module %s\n" name;
  let (tycons, vals, exts) = parse defs in
  Library.register { Module.parent = None; name; tycons; vals; exts }

let compile_file' src =
  let open Source in
  let mx = Binding.of_string src.mod_name in
  let typed = Typing.f & Naming.f mx src.defs in
  let prog = Erlang.f & Closure.f & Alpha.f & KNormal.f typed in
  let outbuf = Buffer.create 128 in
  Emit.f src.erl_name outbuf prog;
  let outchan = open_out src.erl_path in
  Buffer.output_buffer outchan outbuf;
  close_out outchan;
  register src.mod_name typed;

  if !Config.gen_sig_file then
    gen_sig_file src.path typed;

  if not !Config.compile_only then begin
    if !Config.escript then
      create_exec_file src.erl_path
    else
      compile_erl_file src.erl_path;
    Unix.unlink src.erl_path
  end

let compile_file fpath =
  let src = Source.parse fpath in
  if not !Config.syntax_only then
    compile_file' src
