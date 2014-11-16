type t = Type.t M.t

let import t m =
  M.add_list (Module.fun_typs m) t

let create () =
  let env = M.empty in
  import env (Context.find_module "Pervasives")
