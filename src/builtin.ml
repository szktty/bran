open Type

let lib m f = Printf.sprintf "bran_lib_%s:%s" m f

let create name fs =
  let lib' = lib (String.lowercase name) in
  let m = Module.create name [] in
  List.iter (fun (x, args, ret, ext) ->
               let f = { Type.fun_mod = Some m;
                         fun_ext = Some (lib' ext);
                         fun_name = Some x;
                         fun_args = args;
                         fun_ret = ret }
               in
               Module.add_fun m x f) fs;
  m

let pervasives =
  create "Pervasives" [
    ("print_string", [String], Unit, "print_string");
    ("print_int", [Int], Unit, "print_int");
  ]

let erlang =
  create "Erlang" [
    ("eval", [String], String, "eval");
  ]

let init () =
  let ms = [pervasives; erlang] in
  List.iter Context.add_module ms
