open Base

exception Error of Location.t * string
exception Pervasives_not_found

let find_lib_file path =
  let open Spotlib.Filepath in
  match List.find_map_opt
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

let parse src =
  let open Ast_t in
  let open Locating in
  let parse' (typs, vals, exts) def =
    match def.desc with
    | SigDef { sig_name = (x, t); sig_ext = sig_ext } ->
      Log.debug "# val %s : %s\n" x (Type.to_string t);
      let exts' =
        match sig_ext with
        | None -> exts
        | Some s -> (x, s) :: exts
      in
      (typs, (x, t) :: vals, exts')
    | _ ->
      raise (Error (def.loc, "Signature definition only at .bri file"))
  in
  List.fold_left parse' ([], [], []) src.Source.defs

let load src =
  let name = src.Source.mod_name in
  Log.verbose "# begin loading module %s\n" name;
  let (tycons, vals, exts) = parse src in
  Library.register { Module.parent = None; name; tycons; vals; exts };
  Log.verbose "# end loading module %s\n" name

let load_file fpath = 
  Log.verbose "# loading signature file \"%s\"...\n" fpath;
  let fpath' = find_lib_file fpath in
  Log.verbose "#    load %s\n" fpath';
  if Sys.file_exists fpath' then begin
    load & Source.parse fpath';
    true
  end else
    false

let load_module path =
  match Binding.path_name path with
  | Some _, _ -> failwith "not yet supported"
  | None, name -> load_file & String.uncapitalize name ^ ".bri"

let empty = ref None

let create_env () =
  match !empty with
  | Some env -> env
  | None ->
    let env = !Env.empty in
    if not & load_module Binding.pervasives then
      raise Pervasives_not_found;
    let env' = Env.import env & Library.find_module Binding.pervasives in
    empty := Some env';
    env'
