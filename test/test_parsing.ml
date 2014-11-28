open Spotlib.Base
open Sealing
open OUnit2
open Xounit

let setup _test_ctx = ()

let teardown _ctx _test_ctx = ()

let test_parsing file test_ctx =
  let _ctx = bracket setup teardown test_ctx in
  let res =
    run
      ~start_clear:true
      (fun env ->
         let dest = Env.install env file in
         Program.command @ ["-s"; dest])
  in
  assert_success res;
  assert_no_file_changes res

let parsing_files = [
  ("simple_def", "def.br");
  ("atom", "atom.br");
  ("string", "string.br");
  ("bool", "bool.br");
]

let suite =
  let auto =
    List.rev & List.map (fun (name, file) ->
        name >:: (test_parsing & "../parsing/" ^ file))
      parsing_files
  in
  "parsing" >: (test_list auto)
