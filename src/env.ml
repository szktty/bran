type t = { 
  venv   : Type.t M.t; 
  tenv   : Type.t M.t; 
  tycons : Type.tycon M.t 
}

let predeftypes = [
    ("bool", Type.TyFun([], Type.App(Type.Bool, [])));
    ("int", Type.TyFun([], Type.App(Type.Int, [])));
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
    
let add_var env x t = { env with venv = M.add x t env.venv }
let exists_tycon { tycons = tycons } x = M.mem x tycons
let find_tycon { tycons = tycons } x = M.find x tycons
  
