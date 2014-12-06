open Base

let modules = ref []

let register m =
  modules := m :: !modules

let find_opt x =
  List.find_opt (fun m -> m.Module.name = x) !modules

let find x =
  match find_opt x with
  | None -> raise Not_found
  | Some m -> m

let mem x = find_opt x <> None

let find_type_opt (path, base) =
  match begin
    match path with
    | [] -> find_opt "Pervasives"
    | [x] -> find_opt x
    | _ -> None (* not yet support *)
  end with
  | None -> None
  | Some m -> Module.find_val_opt m base
