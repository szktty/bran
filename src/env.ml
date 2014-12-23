open Base

type t = {
  venv   : Type_t.t Id.Map.t;
  tenv   : Type_t.t Id.Map.t;
  tycons : Type_t.tycon Id.Map.t;
  mods : Module.t list;
}

let empty = ref { 
  venv   = Id.Map.empty; 
  tenv   = Id.Map.empty;
  tycons = Id.Map.empty;
  mods = [];
}

(* create empty environment *)
let () =
  empty := List.fold_left 
    (fun { venv = venv; tenv = tenv; tycons = tycons; mods = mods } (x, t) ->
      { venv = Id.Map.add_alist (Type.Tycon.vars t) venv;
        tenv = Id.Map.add_alist (Type.Tycon.types t) tenv;
        tycons = Id.Map.add x t tycons;
        mods = mods }) !empty Library.builtin_tycons
    
let add_tycon env x t = { env with tycons = Id.Map.add x t env.tycons }
let add_var env x t = { env with venv = Id.Map.add x t env.venv }
let add_vars env xys = List.fold_left (fun env (x, y) -> add_var env x y) env xys
let find_var { venv = venv } x = Id.Map.find x venv
let exists_tycon { tycons = tycons } x = Id.Map.mem x tycons
let find_tycon { tycons = tycons } x = Id.Map.find x tycons

let find_var_opt { venv = venv } x =
  if Id.Map.mem x venv then
    Some (Id.Map.find x venv)
  else
    None

let import env m =
  let fold f = List.fold_left (fun env (x, t) -> f env x t) in
  let env' = fold add_tycon env m.Module.tycons in
  let env'' = fold add_var env' m.Module.vals in
  { env'' with mods = m :: env''.mods }

let find_module_of_val_opt env x =
  List.find_opt (fun m ->
      match Module.find_val_opt m x with
      | Some _ -> true
      | None -> false) env.mods

let find_module_of_val env x =
  Spotlib.Option.from_Some & find_module_of_val_opt env x

let is_module_val env x = find_module_of_val_opt env x <> None
