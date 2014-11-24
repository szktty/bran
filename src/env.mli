type t = {
  venv   : Type.t M.t;
  tenv   : Type.t M.t;
  tycons : Type.tycon M.t;
  mods : Module.t list;
}

val empty : t ref
val add_typ : t -> M.key -> Type.tycon -> t
val add_var : t -> M.key -> Type.t -> t
val add_vars : t -> (M.key * Type.t) list -> t
val find_var_opt : t -> M.key -> Type.t option
val find_var : t -> M.key -> Type.t
val exists_tycon : t -> M.key -> bool
val find_tycon : t -> M.key -> Type.tycon
val import : t -> Module.t -> t
val find_module_of_val_opt : t -> Id.t -> Module.t option
val find_module_of_val : t -> Id.t -> Module.t
val is_module_val : t -> Id.t -> bool
