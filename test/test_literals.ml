open Spotlib.Base
open OUnit2
open Xounit

let setup _test_ctx = ()

let teardown _ctx _test_ctx = ()

let test_compile file expect test_ctx =
  let _ctx = bracket setup teardown test_ctx in
  let env = Sealing.Env.create () in
  Sealing.Env.run env
    (fun env ->
       let beam = Program.compile env file in
       let res = Program.call_main env beam in
       assert_success res;
       assert_equal expect res.stdout;
       ())

let test_files = [
  ("true", "true.br", "true");
  ("false", "false.br", "false");
  ("char", "test_char.br", "97"); (* 'a' *)
  ("string", "test_string.br", "\"test\"");
]

let suite =
  let auto =
    List.rev & List.map (fun (name, file, expect) ->
        name >:: (test_compile ("../literals/" ^ file) expect))
      test_files
  in
  "literals" >: (test_list auto)
