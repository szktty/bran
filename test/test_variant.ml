open Spotlib.Base
open Sealing
open OUnit2

let setup _test_ctx = ()

let teardown _ctx _test_ctx = ()

let test_file file test_ctx =
  let _ctx = bracket setup teardown test_ctx in
  run
    ~start_clear:true
    (fun env ->
       let beam = Program.compile env file in
       Program.simple_test env beam)

let test_files = [
  ("noargs", "test_noargs.br");
  ("args", "test_args.br");
]

let suite =
  let auto =
    List.rev & List.map (fun (name, file) ->
        name >:: (test_file ("../variant/" ^ file)))
      test_files
  in
  "variant" >: (test_list auto)
