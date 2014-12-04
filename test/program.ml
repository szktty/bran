open Spotlib.Base
open OUnit2
open Xounit

type beam = string

let prog = "../../src/bran"
let flags = ["-I"; "../../lib"]
let command = List.concat [[prog]; flags]

let erl_flags = ["-pa"; "../../liberl/ebin"; "-pa"; ".."]

let beam_path path =
  Sealing.replace_extension path ".beam"

let exec_path path =
  fst & Spotlib.Xfilename.split_extension path

let compile env file =
  let dest = Sealing.Env.install env file in
  let res = Sealing.Env.shell env (command @ [dest]) in
  assert_success res;
  beam_path dest

let eval env beam code =
  let args =
    ["erl"; "-boot"; "start_clean"; "-noinput";
     "-s"; "init"; "stop"; "-eval"; code] @ erl_flags
  in
  Sealing.Env.shell env args

let simple_test env beam =
  let (modname, _) = Spotlib.Xfilename.split_extension beam in
  let res = eval env beam & Printf.sprintf "testlib:simple_test(%s)" modname in
  assert_success res;
  assert_equal "true" res.stdout
