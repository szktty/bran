open Spotlib.Base
open Sealing
open OUnit2
open Xounit

let setup _test_ctx = ()

let teardown _ctx _test_ctx = ()

let test_parsing file expect test_ctx =
  let _ctx = bracket setup teardown test_ctx in
  let res =
    run
      ~start_clear:true
      (fun env ->
         let dest = Env.install env file in
         Env.shell env & Program.command @ ["-print-type"; "dummy"; dest])
  in
  assert_success res;
  assert_equal expect res.stdout

let parsing_files = [
  ("unit", "unit.bri", "unit");
]

let suite =
  let auto =
    List.rev & List.map (fun (name, file, expect) ->
        name >:: (test_parsing ("../typexp/" ^ file) expect))
      parsing_files
  in
  "parsing" >: (test_list auto)
