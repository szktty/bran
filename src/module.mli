type t = {
  name : Id.t;
  typs : (Id.t * Type_t.tycon) list;
  vals : (Id.t * Type_t.t) list;
  exts : (Id.t * string) list;
}

val modules : t list ref
val register : t -> unit
val mem : Id.t -> bool
val find_opt : Id.t -> t option
val find : Id.t -> t

val find_typ_opt : t -> Id.t -> Type_t.tycon option
val find_val_opt : t -> Id.t -> Type_t.t option
val find_val : t -> Id.t -> Type_t.t
val find_ext_opt : t -> Id.t -> string option

val erl_name : t -> string
val primitive : t -> Id.t -> string
