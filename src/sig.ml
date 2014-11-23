exception Error of Location.t * string

let parse defs =
  let open Syntax in
  let open Locating in
  let parse' (typs, vals) def =
    match def.desc with
    | SigDef { sig_name = (x, t) } ->
      Log.debug "# val %s : %s\n" x (Type.string_of_t t);
      (typs, (x, t) :: vals)
    | _ ->
      raise (Error (def.loc, "Signature definition only at .bri file"))
  in
  List.fold_left parse' ([], []) defs

let load name defs =
  Log.verbose "# begin loading module %s\n" name;
  let (typs, vals) = parse defs in
  Module.register { Module.name; typs; vals };
  Log.verbose "# end loading module %s\n" name;
