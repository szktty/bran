open Base

let modules = ref []

let register m =
  modules := m :: !modules

let find_module_opt x =
  List.find_opt (fun m -> Module.path m = x) !modules

let find_module x =
  match find_module_opt x with
  | None -> raise Not_found
  | Some m -> m

let path_name path =
  match Binding.path_name path with
  | None, x -> Binding.pervasives, x
  | Some path, x -> path, x

let mem_module x = find_module_opt x <> None

let find_tycon_opt path =
  let path, x = path_name path in
  Module.find_tycon_opt (find_module path) x

let find_val_opt path =
  let path, x = path_name path in
  Module.find_val_opt (find_module path) x

let find_val path =
  match find_val_opt path with
  | None -> raise Not_found
  | Some t -> t

(* builtin types *)

let predefloc x =
  With.Loc.create Location.zero x

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

let builtin_tycons = [
  ("unit", tyfun_app Type_t.Unit);
  ("bool", tyfun_app Type_t.Bool);
  ("int", tyfun_app Type_t.Int);
  ("float", tyfun_app Type_t.Float);
  ("char", tyfun_app Type_t.Char);
  ("string", tyfun_app Type_t.String);
  ("atom", tyfun_app Type_t.Atom);
  ("bitstring", tyfun_app Type_t.Bitstring);
  ("binary", tyfun_app Type_t.Binary);
  ("list", tyfun ["a"] & app Type_t.List []);
]
