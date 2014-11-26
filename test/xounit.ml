open Spotlib.Base
open Sealing
open OUnit2

let assert_success res =
  assert_bool "not normally terminated" & Result.is_succeeded res

let assert_no_file_changes res =
  assert_bool "any file changed" (not & Result.has_file_changes res)
