open Spotlib.Base
open Sealing
open OUnit2
open Xounit

let setup _test_ctx = ()

let teardown _ctx _test_ctx = ()

let test_parsing file test_ctx =
  let _ctx = bracket setup teardown test_ctx in
  let res =
    run
      ~start_clear:true
      (fun env ->
         let dest = Env.install env file in
         Program.command @ ["-s"; dest])
  in
  assert_success res;
  assert_no_file_changes res

let parsing_files = [
  ("comment", "comment.br");

  (* definitions *)
  ("top_def", "def.br");
  ("top_var", "topvar.br");
  ("local_def", "local_def.br");
  ("local_var", "local_var.br");
  ("circular_ref", "circular_ref.br");

  (* signatures *)
  (*("sig_def", "sig_def.bri");*)
  (*("sig_var", "sig_var.bri");*)
  ("external", "external.bri");

  (* literals *)
  ("atom", "atom.br");
  ("char", "char.br");
  ("string", "string.br");
  ("bool", "bool.br");
  ("int", "int.br");
  ("float", "float.br");
  ("bitstring", "bitstring.br");
  ("dollar", "dollar.br");
  ("list", "list.br");
  ("tuple", "tuple.br");
  ("array", "array.br");
  ("field", "field.br");

  (* operators *)
  ("intop", "intop.br");
  ("floatop", "floatop.br");

  (* pattern matching *)
  ("match", "match.br");
  ("pattern", "pattern.br");
  ("message", "message.br");

  (* controls *)
  ("if", "if.br");
  ("for", "for.br");
  ("try", "try.br");
  ("fun", "fun.br");
  ("monad", "monad.br");
  ("assert", "assert.br");
  ("exception", "exception.br");
  ("ref", "ref.br");

  (* type definitions *)
  ("simple_typdef", "simple_typdef.br");
  ("variant_typdef", "variant_typdef.br");
  
]

let suite =
  let auto =
    List.rev & List.map (fun (name, file) ->
        name >:: (test_parsing & "../parsing/" ^ file))
      parsing_files
  in
  "parsing" >: (test_list auto)
