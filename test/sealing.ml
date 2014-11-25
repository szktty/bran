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

  let change_to_string = function
    | Not_changed -> "Not_changed"
    | Accessed -> "Accessed"
    | Created -> "Created"
    | Modified -> "Modified"
    | Changed -> "Changed"
    | Deleted -> "Deleted"

  let to_string ch =
    Printf.sprintf "(%s, %s, %f)" ch.path (change_to_string ch.change) ch.time

end

module Result = struct

  type t = {
    stdout : string;
    stderr : string;
    status : Unix.process_status;
    file_changes : FileChange.t list;
    predictions : FileChange.t list;
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

  let files_updated res =
    List.filter (fun fc ->
        match fc.FileChange.change with
        | FileChange.Not_changed -> false
        | _ -> true)
      res.file_changes

  let has_file_changes res =
    List.length (files_updated res) > 0

  let has_files_created_only res paths =
    let open FileChange in
    let (created, all) =
      List.fold_left
        (fun (accu, all) fc ->
           match fc.change with
           | Not_changed | Accessed -> accu, all
           | Created -> fc.path :: accu, all
           | _ -> accu, false)
        ([], true) res.file_changes
    in
    if not all || (List.length created <> List.length paths) then
      false
    else
      List.for_all (fun p -> List.mem p created) paths

  let return_code res =
    match res.status with
    | Unix.WEXITED v -> Some v
    | _ -> None

  let is_exited res =
    match res.status with
    | Unix.WEXITED _ -> true
    | _ -> false

  let is_succeeded res =
    match res.status with
    | Unix.WEXITED 0 -> true
    | _ -> false

  let prediction res =
    let open FileChange in
    let (related, others) =
      List.partition
        (fun ex -> List.exists (fun ac -> ex.path = ac.path) res.file_changes)
        res.predictions
    in
    if not & List.for_all
         (fun fc -> fc.change = Not_changed || fc.change = Accessed) others then
      `Failure
    else
      if List.for_all
          (fun ex -> List.exists
              (fun ac -> ex.path = ac.path && ex.change = ac.change)
              related)
          res.predictions then
        `Success
      else
        `Failure
 
end

module Env = struct

  module Vars = Map.Make(String)

  type t = {
    mutable running : bool;
    origdir : string;
    basedir : string;
    mutable workdir : string;
    vars : string Vars.t;
    ignore_files : Str.regexp list;
    ignore_hidden : bool;
    mutable file_changes : FileChange.t list;
    mutable predictions : FileChange.t list;
  }

  let run_check env =
    if not env.running then
      failwith "Sealing.Env: process is not running"

  let not_run_check env =
    if env.running then
      failwith "Sealing.Env: process is running"

  let ignore env path =
    let (dir, base) =
      match Filepath.dirbase & Filepath.of_string Filepath.os path with
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
    (* FIXME: ~force is for whether delete files not managed by the env or not *)
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
    Xunix.with_chdir env.origdir
      (fun () ->
         Xunix.with_chdir env.basedir
           (fun () -> List.iter (rm ~force env) & read_dir env "."))

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
      | None -> basedir
      | Some s -> s
    in
    let ignore_files' = List.map Str.regexp ignore_files in
    let e = { running = false; origdir = Unix.getcwd ();
              basedir; workdir; vars;
              ignore_files = ignore_files';
              ignore_hidden; file_changes = []; predictions = [] } in
    if start_clear then
      clear e;
    if not & Sys.file_exists basedir then
      Unix.mkdir basedir 0o744;
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
    let files = read_all_files env "." in
    let changes =
      List.fold_left
        (fun accu fc ->
           if not & List.mem fc.path files then
             { fc with change = Deleted; time = Unix.time () } :: accu
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
                 { fc with change = Not_changed; time = Unix.time () }
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

  let print_outerr outbuf errbuf =
    Printf.printf "stdout: ";
    Buffer.output_buffer stdout outbuf;
    Printf.printf "\nstderr: ";
    Buffer.output_buffer stderr errbuf;
    Printf.printf "\n";
    flush_all ()

  let run
      ?(expect_error=true)
      ?(expect_stderr=false)
      ?chdir
      ?(quiet=false)
      env
      ~f =
    not_run_check env;
    env.running <- true;
    let chdir =
      match chdir with
      | None -> env.basedir
      | Some d -> d
    in
    Xunix.with_chdir chdir
      (fun () ->
         let args = f env in
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
                       if not expect_stderr then begin
                         Buffer.add_string errbuf s;
                         print_outerr outbuf errbuf;
                         failwith "Sealing.Env.run: stderr is not expected"
                       end else
                         errbuf
                   in
                   Buffer.add_string buf s)
         in
         if not expect_error && st <> (Unix.WEXITED 0) then
           failwith "Sealing.Env.run: error is not expected";
         if not quiet && st <> (Unix.WEXITED 0) then
           print_outerr outbuf errbuf;
         update_file_changes env;
         env.running <- false;
         { Result.stdout = Buffer.contents outbuf;
           stderr = Buffer.contents errbuf;
           status = st;
           file_changes = env.file_changes;
           predictions = env.predictions;
         })

  let install env src =
    run_check env;
    let dest =
      match Filepath.dirbase & Filepath.of_string Filepath.os src with
      | _, None -> failwith "cannot copy root directory"
      | _, Some base -> base
    in
    match Sys.command & Printf.sprintf "cp %s ." src with
    | 0 ->
      env.file_changes <- { FileChange.path = dest;
                            change = Not_changed;
                            time = Unix.time () }
                          :: env.file_changes;
      dest
    | v -> Exn.failwithf "Env.install: copying file %s failed (exit %d)" src v

  let write env path f =
    with_oc (open_out path) f

  let predict env path change =
    env.predictions <- { FileChange.path; change; time = 0.0 } :: env.predictions

end

let run 
    ?env
    ?chdir
    ?start_clear
    ?ignore_files
    ?ignore_hidden
    ?expect_error
    ?expect_stderr
    ?chdir
    ?quiet
    ?(basedir="test_output")
    f =
  let env = Env.create ?env ?chdir ?start_clear ?ignore_files
      ?ignore_hidden basedir in
  Env.run ?expect_error ?expect_stderr ?chdir ?quiet env ~f

let replace_extension path ext =
    (fst & Xfilename.split_extension path) ^ ext

let _test () =
  let res = run (fun _env -> ["ls"]) in
  print_endline res.stdout

(* let _ = _test () *)
