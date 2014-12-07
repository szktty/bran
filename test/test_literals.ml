open Spotlib.Base
open OUnit2

let setup _test_ctx = ()

let teardown _ctx _test_ctx = ()

let test_compile file test_ctx =
  let _ctx = bracket setup teardown test_ctx in
  let env = Sealing.Env.create () in
  Sealing.Env.run env
    (fun env ->
       let beam = Program.compile env file in
       Program.simple_test env beam)

let test_files = [
  ("true", "test_true.br");
  ("false", "test_false.br");
  ("char", "test_char.br");
  ("string", "test_string.br");
  ("atom", "test_atom.br");
  ("atom_quote", "test_atom_quote.br");
  ("int", "test_int.br");
  ("float", "test_float.br");
  ("bitstring", "test_bitstring.br");
]

let suite =
  let auto =
    List.rev & List.map (fun (name, file) ->
        name >:: test_compile ("../literals/" ^ file))
      test_files
  in
  "literals" >: (test_list auto)
