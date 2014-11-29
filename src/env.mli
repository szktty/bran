type t = {
  venv   : Type_t.t M.t;
  tenv   : Type_t.t M.t;
  tycons : Type_t.tycon M.t;
  mods : Module.t list;
}

val empty : t ref
val add_typ : t -> M.key -> Type_t.tycon -> t
val add_var : t -> M.key -> Type_t.t -> t
val add_vars : t -> (M.key * Type_t.t) list -> t
val find_var_opt : t -> M.key -> Type_t.t option
val find_var : t -> M.key -> Type_t.t
val exists_tycon : t -> M.key -> bool
val find_tycon : t -> M.key -> Type_t.tycon
val import : t -> Module.t -> t
val find_module_of_val_opt : t -> Id.t -> Module.t option
val find_module_of_val : t -> Id.t -> Module.t
val is_module_val : t -> Id.t -> bool
