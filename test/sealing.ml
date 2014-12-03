open Spotlib
open Spotlib.Base

let debug = ref false

let printf = function
  | false -> Spotlib.Xprintf.zprintf
  | true ->
    flush_all ();
    Printf.printf "# Sealing: pid %d: "( Unix.getpid ());
    Printf.printf

let dprintf f = printf !debug f

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
    let open Spotlib.Temporal in
    Printf.sprintf "(%s, %s, %s)" ch.path (change_to_string ch.change)
      (Datetime.to_string & Datetime.of_utc_tm & Unix.localtime ch.time)

end

module Result = struct

  type t = {
    stdout : string;
    stderr : string;
    status : Unix.process_status;
    file_changes : FileChange.t list;
    predictions : FileChange.t list;
  }

  let changes res =
    List.filter (fun fc -> fc.FileChange.change <> Not_changed) res.file_changes

  let find_files_changed res ch =
    List.map FileChange.path &
      List.filter (fun fc -> fc.FileChange.change = ch) res.file_changes

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
    origdir : string;
    basedir : string;
    vars : string Vars.t;
    ignore_files : Str.regexp list;
    ignore_hidden : bool;
    expect_error : bool;
    expect_stderr : bool;
    quiet : bool;
    mutable file_changes : FileChange.t list;
    mutable predictions : FileChange.t list;
  }

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

  (* initialize PRNG for parallel execution *)
  let () = Random.self_init ()
  let gen_id () =
    Printf.sprintf "%x" (Random.bits ())

  let create
      ?env
      ?(start_clear=false)
      ?(ignore_files=[])
      ?(ignore_hidden=true)
      ?(parallel=true)
      ?(expect_error=true)
      ?(expect_stderr=false)
      ?(quiet=false)
      ?(basedir="test_output")
      () =
    let basedir' =
      if parallel then
        let rec gendir () =
          let dir = Printf.sprintf "%s-%s" basedir (gen_id ()) in
          if Sys.file_exists dir then
            gendir ()
          else
            dir
        in
        gendir ()
      else
        basedir
    in
    dprintf "create: %s\n" basedir';
    let vars =
      match env with
      | None -> Vars.empty
      | Some kvs ->
        List.fold_left (fun vars (k, v) -> Vars.add k v vars) Vars.empty kvs
    in
    let ignore_files' = List.map Str.regexp ignore_files in
    let e = { origdir = Unix.getcwd ();
              basedir = basedir'; vars;
              ignore_files = ignore_files';
              ignore_hidden; expect_error; expect_stderr;
              quiet; file_changes = []; predictions = [] }
    in
    if start_clear && Sys.file_exists basedir' then
      clear e;
    if not & Sys.file_exists basedir' then begin
      match Spotlib.Xunix.mkdir basedir' ~perm:0o744 ~recursive:true with
      | _ -> ()
    end;
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

  let init_file_changes env =
    List.iter (fun path ->
                 env.file_changes <- { FileChange.path;
                                       change = FileChange.Not_changed;
                                       time = Unix.time () } :: env.file_changes)
      & read_all_files env env.basedir

  let run ?chdir env f =
    let chdir =
      match chdir with
      | None -> env.basedir
      | Some d -> d
    in
    init_file_changes env;
    Xunix.with_chdir chdir
      (fun () ->
         let ret = f env in
         update_file_changes env;
         ret)

  let shell env args =
    dprintf "$ %s\n" (String.concat " " args);
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
                  if not env.expect_stderr then begin
                    Buffer.add_string errbuf s;
                    print_outerr outbuf errbuf;
                    failwith "Sealing.Env.run: stderr is not expected"
                  end else
                    errbuf
              in
              dprintf "out: '%s'\n" s;
              Buffer.add_string buf s)
    in
    if not env.expect_error && st <> (Unix.WEXITED 0) then
      failwith "Sealing.Env.run: error is not expected";
    if not env.quiet && st <> (Unix.WEXITED 0) then
      print_outerr outbuf errbuf;
    update_file_changes env;
    { Result.stdout = Buffer.contents outbuf;
      stderr = Buffer.contents errbuf;
      status = st;
      file_changes = env.file_changes;
      predictions = env.predictions;
    }

  let install env src =
    let dest =
      match Filepath.dirbase & Filepath.of_string Filepath.os src with
      | _, None -> failwith "cannot copy root directory"
      | _, Some base -> base
    in
    dprintf "install: cp %s .\n" src;
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
    ?start_clear
    ?ignore_files
    ?ignore_hidden
    ?parallel
    ?expect_error
    ?expect_stderr
    ?quiet
    ?basedir
    f =
  let env = Env.create ?env ?start_clear ?ignore_files
      ?ignore_hidden ?parallel ?basedir
      ?expect_error ?expect_stderr ?quiet ()
  in
  Env.run env f

let replace_extension path ext =
    (fst & Xfilename.split_extension path) ^ ext

let _test () =
  let res = run (fun env -> Env.shell env ["ls"]) in
  print_endline res.stdout

(* let _ = _test () *)
