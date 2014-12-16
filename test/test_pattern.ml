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
  ("var", "test_var.br");
  ("bool", "test_bool.br");
  ("atom", "test_atom.br");
  ("int", "test_int.br");
  ("string", "test_string.br");
  ("unit", "test_unit.br");
  ("tuple", "test_tuple.br");
  ("list", "test_list.br");
  ("empty_list", "test_empty_list.br");
  ("one_list", "test_one_list.br");
  ("cons", "test_cons.br");
  ("as", "test_as.br");
  ("constr", "test_constr.br");
  ("constr_args", "test_constr_args.br");
]

let suite =
  let auto =
    List.rev & List.map (fun (name, file) ->
        name >:: (test_file ("../pattern/" ^ file)))
      test_files
  in
  "pattern" >: (test_list auto)
