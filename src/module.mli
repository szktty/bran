type t = {
  parent : t option;
  name : Id.t;
  tycons : (Id.t * Type_t.tycon) list;
  vals : (Id.t * Type_t.t) list;
  exts : (Id.t * string) list;
}

val path : t -> Binding.t
val find_tycon_opt : t -> Id.t -> Type_t.tycon option
val find_val_opt : t -> Id.t -> Type_t.t option
val find_val : t -> Id.t -> Type_t.t
val find_ext_opt : t -> Id.t -> string option

val erl_name : t -> string
val primitive : t -> Id.t -> string
