open Spotlib.Base

type t = {
  venv   : Type.t M.t;
  tenv   : Type.t M.t;
  tycons : Type.tycon M.t;
  mods : Module.t list;
}

let predeftypes = [
    ("unit", Type.TyFun([], Type.App(Type.Unit, [])));
    ("bool", Type.TyFun([], Type.App(Type.Bool, [])));
    ("int", Type.TyFun([], Type.App(Type.Int, [])));
    ("string", Type.TyFun([], Type.App(Type.String, [])));
    ("list", Type.TyFun(["a"], Type.App(Type.Variant("list", [("Nil", []); ("Cons", [Type.Var("a"); Type.App(Type.NameTycon("list", ref None), [Type.Var("a")])])]), [])));
]

let empty = ref { 
  venv   = M.empty; 
  tenv   = M.empty;
  tycons = M.empty;
  mods = [];
}

let () =
  empty := List.fold_left 
    (fun { venv = venv; tenv = tenv; tycons = tycons; mods = mods } (x, t) ->
      { venv = M.add_list (Type.vars t) venv;
        tenv = M.add_list (Type.types t) tenv;
        tycons = M.add x t tycons;
        mods = mods }) !empty predeftypes
    
let add_typ env x t = { env with tycons = M.add x t env.tycons }
let add_var env x t = { env with venv = M.add x t env.venv }
let add_vars env xys = List.fold_left (fun env (x, y) -> add_var env x y) env xys
let find_var { venv = venv } x = M.find x venv
let exists_tycon { tycons = tycons } x = M.mem x tycons
let find_tycon { tycons = tycons } x = M.find x tycons

let find_var_opt { venv = venv } x =
  if M.mem x venv then
    Some (M.find x venv)
  else
    None

let import env m =
  let fold f = List.fold_left (fun env (x, t) -> f env x t) in
  let env' = fold add_typ env m.Module.typs in
  let env'' = fold add_var env' m.Module.vals in
  { env'' with mods = m :: env''.mods }

let find_module_of_val_opt env x =
  Spotlib.Xlist.find_opt (fun m ->
      match Module.find_val_opt m x with
      | Some _ -> true
      | None -> false) env.mods

let find_module_of_val env x =
  Spotlib.Option.from_Some & find_module_of_val_opt env x

let is_module_val env x = find_module_of_val_opt env x <> None
