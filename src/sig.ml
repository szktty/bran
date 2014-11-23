exception Error of Location.t * string

let parse defs =
  let open Syntax in
  let open Locating in
  let parse' (typs, vals, exts) def =
    match def.desc with
    | SigDef { sig_name = (x, t); sig_ext = sig_ext } ->
      Log.debug "# val %s : %s\n" x (Type.string_of_t t);
      let exts' =
        match sig_ext with
        | None -> exts
        | Some s -> (x, s) :: exts
      in
      (typs, (x, t) :: vals, exts')
    | _ ->
      raise (Error (def.loc, "Signature definition only at .bri file"))
  in
  List.fold_left parse' ([], [], []) defs

let load name defs =
  Log.verbose "# begin loading module %s\n" name;
  let (typs, vals, exts) = parse defs in
  Module.register { Module.name; typs; vals; exts };
  Log.verbose "# end loading module %s\n" name;
