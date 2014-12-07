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
  ("unit", "unit.bri", "unit");
  ("bool", "bool.bri", "bool");
  ("char", "char.bri", "char");
  ("int", "int.bri", "int");
  ("float", "float.bri", "float");
  ("atom", "atom.bri", "atom");
  ("string", "string.bri", "string");
  ("bitstring", "bitstring.bri", "bitstring");
  ("binary", "binary.bri", "binary");
  ("tuple", "tuple.bri", "(int * int)");

  (* type functions *)
  ("list", "list.bri", "string list");

  (* functions *)
  ("fun_of_unit_unit", "fun_of_unit_unit.bri", "unit -> unit");
  ("fun_of_int_float_string", "fun_of_int_float_string.bri",
   "int -> float -> string");
  ("fun_of_fun", "fun_of_fun.bri", "(char -> bool) -> string -> string list");

  (* polymorphic types *)
  ("poly_list", "poly_list.bri", "'a list");
  ("poly_fun", "poly_fun.bri", "('a -> 'b) -> 'b list -> 'b");
]

let suite =
  let auto =
    List.rev & List.map (fun (name, file, expect) ->
        name >:: (test_parsing ("../typexp/" ^ file) expect))
      parsing_files
  in
  "typexp" >: (test_list auto)
