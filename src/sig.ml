open Spotlib.Base

exception Error of Location.t * string
exception Pervasives_not_found

let find_lib_file path =
  let open Spotlib in
  let open Filepath in
  match Xlist.find_map_opt
          (fun dir ->
             let dir' = of_string os dir in
             let path' = to_string & dir' ^/ path in
             Log.verbose "# find %s\n" path';
             if Sys.file_exists path' then
               Some path'
             else
               None) !Config.load_paths
  with
  | None -> path
  | Some p -> p

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
  Log.verbose "# end loading module %s\n" name

let load_file fpath = 
  Log.verbose "# loading signature file \"%s\"...\n" fpath;
  let fpath' = find_lib_file fpath in
  Log.verbose "#    load %s\n" fpath';
  if Sys.file_exists fpath' then begin
    load (String.capitalize & Utils.module_name fpath) & Utils.parse_file fpath';
    true
  end else
    false

let load_module (name : Id.t) =
  load_file & String.uncapitalize name ^ ".bri"

let empty = ref None

let create_env () =
  match !empty with
  | Some env -> env
  | None ->
    let env = !Env.empty in
    let mx = "Pervasives" in
    if not & load_module mx then
      raise Pervasives_not_found;
    let env' = Env.import env & Module.find mx in
    empty := Some env';
    env'
