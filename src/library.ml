open X

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
