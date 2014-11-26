open Spotlib.Base
open OUnit2

let suites = [
  Test_parsing.suite;
]

let _ =
  run_test_tt_main & test_list suites
