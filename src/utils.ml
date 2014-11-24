open Spotlib.Base

let filepath_of_string path = 
  let open Spotlib.Filepath in
  of_string os path

let dirbase path =
  Spotlib.Filepath.dirbase & filepath_of_string path

let replace_ext path ext =
    (fst & Spotlib.Xfilename.split_extension path) ^ ext

let module_name path =
  match dirbase path with
  | _, None -> failwith "mod_name"
  | _, Some base -> fst & Spotlib.Xfilename.split_extension base

let erl_path path =
  match dirbase path with
  | _, None -> failwith "erl_path"
  | dir, Some base ->
    let open Spotlib.Filepath in
    to_string & dir ^/ replace_ext base ".erl"

let escript_path path =
  let open Spotlib.Filepath in
  match dirbase & of_string os path with
  | _, None -> failwith "modpath"
  | dir, Some base ->
    to_string & dir ^/ (fst & Spotlib.Xfilename.split_extension base)

let parse_file fpath =
  Log.verbose "# Parsing\n";
  let inchan = open_in fpath in
  let prog = Parser.prog Lexer.token (Lexing.from_channel inchan) in
  Log.debug "# %s\n" (String.concat ";\n " (List.map Syntax.string_of_def prog));
  prog
