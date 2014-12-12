open Base

let filepath_of_string path = 
  let open Spotlib.Filepath in
  of_string os path

let dirbase path =
  Spotlib.Filepath.dirbase & filepath_of_string path

let replace_ext path ext =
    (fst & Filename.split_extension path) ^ ext

let base path =
  match dirbase path with
  | _, None -> failwith "base"
  | _, Some base -> fst & Filename.split_extension base

let module_name path =
  String.capitalize & base path

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
    to_string & dir ^/ (fst & Filename.split_extension base)
