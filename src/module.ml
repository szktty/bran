open Base

type t = {
  parent : t option;
  name : Id.t;
  tycons : (Id.t * Type_t.tycon) list;
  vals : (Id.t * Type_t.t) list;
  exts : (Id.t * string) list;
}

let path m =
  let rec f m accu =
    match m.parent with
    | None -> m.name :: accu
    | Some p -> f p & m.name :: accu
  in
  Binding.of_list & f m []

let find_tycon_opt m x =
  List.find_map_opt
    (fun (ex, et) -> if x = ex then Some et else None) m.tycons

let find_val_opt m x =
  List.find_map_opt
    (fun (ex, et) -> if x = ex then Some et else None) m.vals

let find_val m x = find_of_opt & find_val_opt m x

let find_ext_opt m x =
  List.find_map_opt
    (fun (ex, et) -> if x = ex then Some et else None) m.exts

let erl_name m = String.uncapitalize m.name

let primitive m fx =
  match find_ext_opt m fx with
  | Some x -> x
  | None -> erl_name m ^ ":" ^ fx
