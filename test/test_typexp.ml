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
  (* primitive types *)
  ("unit", "test_unit.bri", "unit");
  ("bool", "test_bool.bri", "bool");
  ("char", "test_char.bri", "char");
  ("int", "test_int.bri", "int");
  ("float", "test_float.bri", "float");
  ("atom", "test_atom.bri", "atom");
  ("string", "test_string.bri", "string");
  ("bitstring", "test_bitstring.bri", "bitstring");
  ("binary", "test_binary.bri", "binary");
  ("tuple", "test_tuple.bri", "(int * int)");

  (* type functions *)
  ("list", "test_list.bri", "string list");

  (* functions *)
  ("fun_of_unit_unit", "test_fun_of_unit_unit.bri", "unit -> unit");
  ("fun_of_int_float_string", "test_fun_of_int_float_string.bri",
   "int -> float -> string");
  ("fun_of_fun", "test_fun_of_fun.bri", "(char -> bool) -> string -> string list");

  (* polymorphic types *)
  ("poly_list", "test_poly_list.bri", "'a list");
  ("poly_fun", "test_poly_fun.bri", "('a -> 'b) -> 'b list -> 'b");
]

let suite =
  let auto =
    List.rev & List.map (fun (name, file, expect) ->
        name >:: (test_parsing ("../typexp/" ^ file) expect))
      parsing_files
  in
  "typexp" >: (test_list auto)
