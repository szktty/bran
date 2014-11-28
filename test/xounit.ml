open Spotlib.Base
open Sealing
open OUnit2

let assert_success res =
  assert_bool "not normally terminated" & Result.is_succeeded res

let assert_no_file_changes res =
  assert_bool ("any file changed: " ^
               (String.concat ", " (List.map FileChange.to_string
                                      (Result.changes res))))
    (not & Result.has_file_changes res)
