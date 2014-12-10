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
  ("unit", "test_unit.br", "unit");
  ("bool", "test_bool.br", "bool");
  ("int", "test_int.br", "int");
  ("float", "test_float.br", "float");
  ("atom", "test_atom.br", "atom");
  ("char", "test_char.br", "char");
  ("string", "test_string.br", "string");
  ("bitstring", "test_bitstring.br", "bitstring");
  ("binary", "test_binary.br", "binary");
  ("tuple", "test_tuple.br", "(int * float)");

  (* polymorphic types *)
  ("poly_list", "test_poly_list.br", "'a list");
  ("int_list", "test_int_list.br", "int list");

  (* type constructor *)
  ("simple_tycon", "test_simple_tycon.br", "A | B | C");
  ("tycon_args", "test_tycon_args.br", "A of int | B of bool * unit | C of string list");

  (* record *)
]

let suite =
  let auto =
    List.rev & List.map (fun (name, file, expect) ->
        name >:: (test_file ("../typdef/" ^ file) expect))
      test_files
  in
  "typdef" >: (test_list auto)
