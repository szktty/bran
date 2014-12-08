(* managing modules *)

val modules : Module.t list ref
val register : Module.t -> unit
val mem : Id.t -> bool
val find_opt : Id.t -> Module.t option
val find : Id.t -> Module.t

val find_tycon_opt : Binding.t -> Type_t.tycon option
val find_type_opt : Binding.t -> Type_t.t option

val builtin_tycons : (Id.t * Type_t.tycon) list
