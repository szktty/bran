open Spotlib.Base
open Sealing
open OUnit2
open Xounit

let setup _test_ctx = ()

let teardown _ctx _test_ctx = ()

let test_file file expect test_ctx =
  let _ctx = bracket setup teardown test_ctx in
  let res =
    run
      ~start_clear:true
      (fun env ->
         let dest = Env.install env file in
         Env.shell env & Program.command @ ["-print-tycon"; "t"; dest])
  in
  assert_success res;
  assert_equal expect res.stdout

let test_files = [
  (* monomorphic types *)
  ("int", "test_int.br", "int");

  (* polymorphic types *)
]

let suite =
  let auto =
    List.rev & List.map (fun (name, file, expect) ->
        name >:: (test_file ("../typdef/" ^ file) expect))
      test_files
  in
  "typdef" >: (test_list auto)
