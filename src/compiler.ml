open Spotlib.Base

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
             Typing.f & Utils.parse_file fpath in
  let mname = Utils.module_name fpath in
  let outbuf = Buffer.create 128 in
  Emit.f mname outbuf prog;
  let outfpath =
    if !Config.escript then
      Utils.escript_path fpath
    else
      Utils.erl_path fpath
  in
  let outchan = open_out outfpath in
  Buffer.output_buffer outchan outbuf;
  close_out outchan;

  if not !Config.compile_only then begin
    compile_erl_file outfpath;
    if !Config.escript then
      create_exec_file outfpath
  end
