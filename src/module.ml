type t = Type.module_

let set f m = { f with Type.fun_mod = Some m }

let add_fun m x f =
  m.Type.mod_funs <- (x, set f m) :: m.Type.mod_funs

let create ?(erl="") x fs =
  let erl' = if erl = "" then String.lowercase x else erl in
  let m = { Type.mod_name = x; mod_erl = erl'; mod_funs = [] } in
  List.iter (fun (x, f) -> add_fun m x f) fs;
  m

let of_typ = function
  | Type.Module m -> m
  | _ -> assert false

let find_opt t fx =
  try
    Some (snd (List.find (fun (x, _) -> fx = x) t.Type.mod_funs))
  with
  | Not_found -> None

let find t fx =
  match find_opt t fx with
  | None -> raise Not_found
  | Some ft -> ft

let fun_typs m =
  List.map (fun (x, f) -> (x, Fun.to_typ f)) m.Type.mod_funs
