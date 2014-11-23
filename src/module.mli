type t = {
  name : Id.t;
  typs : (Id.t * Type.tycon) list;
  vals : (Id.t * Type.t) list;
}

val modules : t list ref
val register : t -> unit
val mem : Id.t -> bool
val find_opt : Id.t -> t option
val find : Id.t -> t

val find_typ_opt : t -> Id.t -> Type.tycon option
val find_val_opt : t -> Id.t -> Type.t option
