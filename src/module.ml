open Base

type t = {
  name : Id.t;
  typs : (Id.t * Type_t.tycon) list;
  vals : (Id.t * Type_t.t) list;
  exts : (Id.t * string) list;
}

let find_typ_opt m x =
  List.find_map_opt
    (fun (ex, et) -> if x = ex then Some et else None) m.typs

let find_val_opt m x =
  List.find_map_opt
    (fun (ex, et) -> if x = ex then Some et else None) m.vals

let find_val m x =
  match find_val_opt m x with
  | None -> raise Not_found
  | Some t -> t

let find_ext_opt m x =
  List.find_map_opt
    (fun (ex, et) -> if x = ex then Some et else None) m.exts

let erl_name m = String.uncapitalize m.name

let primitive m fx =
  match find_ext_opt m fx with
  | Some x -> x
  | None -> erl_name m ^ ":" ^ fx
