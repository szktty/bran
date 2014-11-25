open Spotlib
open Spotlib.Base

module FileChange = struct

  type change =
    | Not_changed
    | Accessed
    | Created
    | Modified
    | Changed
    | Deleted

  type t = {
    path : string;
    change : change;
    time : float;
  }

  let path fc = fc.path
  let change fc = fc.change
  let time fc = fc.time

end

module Result = struct

  type t = {
    stdout : string;
    stderr : string;
    status : Unix.process_status;
    file_changes : FileChange.t list;
  }

  let filter_files ch r =
    List.map FileChange.path &
      List.filter (fun fc -> fc.FileChange.change = ch) r.file_changes

  let files_not_changed = filter_files FileChange.Not_changed
  let files_created = filter_files FileChange.Created
  let files_accessed = filter_files FileChange.Accessed
  let files_modified = filter_files FileChange.Modified
  let files_changed = filter_files FileChange.Changed
  let files_deleted = filter_files FileChange.Deleted

end

module Env = struct

  module Vars = Map.Make(String)

  type t = {
    basedir : string;
    mutable workdir : string;
    vars : string Vars.t;
    ignore_files : Str.regexp list;
    ignore_hidden : bool;
    mutable file_changes : FileChange.t list;
  }

  let ignore env path =
    let (dir, base) =
      match Filepath.dirbase
            & Filepath.of_string Filepath.os path
      with
      | _, None -> failwith "cannot delete root directory"
      | dir, Some base -> Filepath.to_string dir, base
    in
    (env.ignore_hidden && Xstring.is_prefix "." base) ||
    (List.exists
       (fun re -> Str.string_match re base 0 || Str.string_match re path 0)
       env.ignore_files)

  let read_dir env path =
    List.filter (not ** ignore env) (Array.to_list & Sys.readdir path)

  let rec rm ~force env path =
    if not & ignore env path then
      match Sys.is_directory path, force with
      | false, _ -> Sys.remove path
      | true, false ->
        Exn.failwithf "%s is not empty" path
      | true, true ->
        List.iter (rm ~force env) & read_dir env path;
        if Array.length (Sys.readdir path) = 0 then
          Unix.rmdir path

  let clear ?(force=false) env =
    rm ~force env env.workdir

  let create
      ?env
      ?chdir
      ?(start_clear=false)
      ?(ignore_files=[])
      ?(ignore_hidden=true)
      basedir =
    let vars =
      match env with
      | None -> Vars.empty
      | Some kvs ->
        List.fold_left (fun vars (k, v) -> Vars.add k v vars) Vars.empty kvs
    in
    let workdir =
      match chdir with
      | None -> Sys.getcwd ()
      | Some s -> s
    in
    let ignore_files' = List.map Str.regexp ignore_files in
    let e = { basedir; workdir; vars; ignore_files = ignore_files';
              ignore_hidden; file_changes = [] } in
    if start_clear then
      clear e;
    if not & Sys.file_exists basedir then
      Unix.mkdir basedir 0o644;
    e

  let read_all_files env path =
    let rec f env path accu =
      List.fold_left (fun accu post ->
          if Sys.is_directory post then
            f env post accu
          else
            post :: accu)
        accu (read_dir env path)
    in
    f env path []

  let find_change_opt env path =
    try
      Some (List.find
              (fun fc -> path = fc.FileChange.path) env.file_changes)
    with
      Not_found -> None

  let update_file_changes env =
    let open FileChange in
    let files = read_all_files env env.basedir in
    let changes =
      List.fold_left
        (fun accu fc ->
           if not & List.mem fc.path files then
             { fc with change = Deleted; time = Sys.time () } :: accu
           else
             accu) [] env.file_changes
    in
    env.file_changes <-
      List.fold_left
        (fun accu path ->
           let stats = Unix.stat path in
           let fc =
             match find_change_opt env path with
             | None ->
               { path; change = Created; time = stats.st_mtime }
             | Some fc ->
               if fc.time >= (max stats.st_atime &
                              max stats.st_mtime stats.st_ctime) then
                 { fc with change = Not_changed; time = Sys.time () }
               else
                 match fc.change with
                 | Deleted ->
                   { fc with change = Created; time = stats.st_mtime }
                 | _ ->
                   if stats.st_mtime >= stats.st_atime &&
                      stats.st_mtime >= stats.st_ctime then
                     { fc with change = Modified; time = stats.st_mtime }
                   else if stats.st_ctime > stats.st_atime &&
                           stats.st_ctime > stats.st_mtime then
                     { fc with change = Changed; time = stats.st_ctime }
                   else
                     { fc with change = Accessed; time = stats.st_atime }
           in
           fc :: accu
        ) changes files

  let run
      ?(expect_error=true)
      ?(expect_stderr=false)
      ?chdir
      ?(quiet=false)
      env args =
    begin match chdir with
      | None -> ()
      | Some d ->
        env.workdir <- d;
        Unix.chdir d
    end;
    let proc = Xunix.Command.execvp args in
    let outbuf = Buffer.create 256 in
    let errbuf = Buffer.create 256 in
    let (st, _) =
      Xunix.Command.iter proc
        ~f:(fun (ch, read) ->
            match read with
            | `EOF -> ()
            | `Read s ->
              let buf =
                match ch with
                | `Out -> outbuf
                | `Err ->
                  if not expect_stderr then
                    failwith "Sealing.Env.run: stderr is not expected"
                  else
                    errbuf
              in
              Buffer.add_string buf s)
    in
    if not expect_error && st <> (Unix.WEXITED 0) then
      failwith "Sealing.Env.run: error is not expected";
    if not quiet && st <> (Unix.WEXITED 0) then begin
      Printf.printf "stdout: ";
      Buffer.output_buffer stdout outbuf;
      Printf.printf "\nstderr: ";
      Buffer.output_buffer stderr errbuf;
      Printf.printf "\n";
      flush_all ();
    end;
    update_file_changes env;
    { Result.stdout = Buffer.contents outbuf;
      stderr = Buffer.contents errbuf;
      status = st;
      file_changes = env.file_changes;
    }

end

let with_run 
    ?env
    ?chdir
    ?start_clear
    ?ignore_files
    ?ignore_hidden
    ?expect_error
    ?expect_stderr
    ?chdir
    ?quiet
    workdir args =
  Env.run ?expect_error ?expect_stderr ?chdir ?quiet
    (Env.create ?env ?chdir ?start_clear ?ignore_files ?ignore_hidden workdir)
    args

let _test () =
  let res = with_run "test_output" ["ls"] in
  print_endline res.stdout

(* let _ = _test () *)
