open Spotlib.Base
open OUnit2

let suites = [
  (* parsing *)
  Test_parsing.suite;
  Test_typexp.suite;

  (* compiling *)
  Test_literals.suite;
  Test_typdef.suite;
  Test_variant.suite;
]

let _ =
  run_test_tt_main & test_list suites
