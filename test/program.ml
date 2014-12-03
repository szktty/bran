open Spotlib.Base
open Xounit

type beam = string

let prog = "../../src/bran"
let flags = ["-I"; "../../libsrc/stdlib"]
let command = List.concat [[prog]; flags]

let erl_flags = ["-pa"; "../../liberl/ebin"]

let beam_path path =
  Sealing.replace_extension path ".beam"

let exec_path path =
  fst & Spotlib.Xfilename.split_extension path

let compile env file =
  let dest = Sealing.Env.install env file in
  let res = Sealing.Env.shell env (command @ [dest]) in
  assert_success res;
  beam_path dest

let call_main env beam =
  let (modname, _) = Spotlib.Xfilename.split_extension beam in
  let args =
    ["erl"; "-boot"; "start_clean"; "-noinput";
     "-s"; "init"; "stop"; "-eval";
     Printf.sprintf "io:format(\"~p\", [%s:main(ok)])" modname]
    @ erl_flags
  in
  Sealing.Env.shell env args
