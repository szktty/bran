type t = {
  name : Id.t;
  typs : (Id.t * Type.tycon) list;
  vals : (Id.t * Type.t) list;
}

let modules = ref []

let register m =
  modules := m :: !modules

let find_opt x =
  Spotlib.Xlist.find_opt (fun m -> m.name = x) !modules

let find x =
  match find_opt x with
  | None -> raise Not_found
  | Some m -> m

let mem x = find_opt x <> None

let find_typ_opt m x =
  Spotlib.Xlist.find_map_opt
    (fun (ex, et) -> if x = ex then Some et else None) m.typs

let find_val_opt m x =
  Spotlib.Xlist.find_map_opt
    (fun (ex, et) -> if x = ex then Some et else None) m.vals
