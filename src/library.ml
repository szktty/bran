open Base

let modules = ref []

let register m =
  modules := m :: !modules

let find_opt x =
  List.find_opt (fun m -> m.Module.name = x) !modules

let find x =
  match find_opt x with
  | None -> raise Not_found
  | Some m -> m

let mem x = find_opt x <> None

let find_tycon_opt (path, base) =
  match begin
    match path with
    | [] -> find_opt "Pervasives"
    | [x] -> find_opt x
    | _ -> assert false (* not yet support *)
  end with
  | None -> None
  | Some m -> Module.find_typ_opt m base

let find_type_opt (path, base) =
  match begin
    match path with
    | [] -> find_opt "Pervasives"
    | [x] -> find_opt x
    | _ -> assert false (* not yet support *)
  end with
  | None -> None
  | Some m -> Module.find_val_opt m base

(* builtin types *)

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

let builtin_tycons = [
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
