open Base

type t = {
  venv   : Type_t.t M.t;
  tenv   : Type_t.t M.t;
  tycons : Type_t.tycon M.t;
  mods : Module.t list;
}

let predefloc x =
  Locating.create Location.zero x

let app tycon ts =
  predefloc (Type_t.App (tycon, ts))

let void_app tycon =
  app tycon []

let tyfun vs t =
  Type_t.TyFun (vs, t)

let void_tyfun t =
  tyfun [] t

let tyfun_app t =
  void_tyfun & void_app t

let predeftypes = [
    ("unit", tyfun_app Type_t.Unit);
    ("bool", tyfun_app Type_t.Bool);
    ("int", tyfun_app Type_t.Int);
    ("float", tyfun_app Type_t.Float);
    ("string", tyfun_app Type_t.String);
    ("atom", tyfun_app Type_t.Atom);
    ("bitstring", tyfun_app Type_t.Bitstring);
    ("binary", tyfun_app Type_t.Binary);
    ("list", tyfun ["a"]
       (void_app
          (Type_t.Variant ("list", [(Type.nil_id, []);
                                    (Type.cons_id,
                                     [predefloc (Type_t.Var "a");
                                      app (Type_t.NameTycon("list", ref None))
                                        [predefloc (Type_t.Var "a")]])]))));
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
      { venv = M.add_list (Type.Tycon.vars t) venv;
        tenv = M.add_list (Type.Tycon.types t) tenv;
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
  List.find_opt (fun m ->
      match Module.find_val_opt m x with
      | Some _ -> true
      | None -> false) env.mods

let find_module_of_val env x =
  Spotlib.Option.from_Some & find_module_of_val_opt env x

let is_module_val env x = find_module_of_val_opt env x <> None
