type t = { 
  venv   : Type.t M.t; 
  tenv   : Type.t M.t; 
  tycons : Type.tycon M.t 
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
  tycons = M.empty
}

let () =
  empty := List.fold_left 
    (fun { venv = venv; tenv = tenv; tycons = tycons } (x, t) -> 
      { venv = M.add_list (Type.vars t) venv;
        tenv = M.add_list (Type.types t) tenv;
        tycons = M.add x t tycons }) !empty predeftypes
    
let add_typ env x t = { env with tycons = M.add x t env.tycons }
let add_var env x t = { env with venv = M.add x t env.venv }
let exists_tycon { tycons = tycons } x = M.mem x tycons
let find_tycon { tycons = tycons } x = M.find x tycons

let import env m =
  let fold f = List.fold_left (fun env (x, t) -> f env x t) in
  let env' = fold add_typ env m.Module.typs in
  fold add_var env' m.Module.vals
