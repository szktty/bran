exception Error of Location.t * string

let rec parse e accu =
  let open AstTypes in
  match Ast.desc e with
  | Nop | Unit -> accu
  | Let (_, e1, e2) -> parse e1 (parse e2 accu)
  | SigDef (x, f) ->
    Log.debug "# val %s : %s\n" x (Fun.to_string f);
    (x, f) :: accu
  | _ ->
    Log.debug "sig error: %s\n" (Ast.to_string e);
    raise (Error (Ast.loc e, "signature definition only at .bri file"))

let load name e =
  Log.verbose "# begin loading module %s\n" name;
  Context.add_module (Module.create name (parse e []));
  Log.verbose "# end loading module %s\n" name;
