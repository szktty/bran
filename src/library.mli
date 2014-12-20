(* managing modules *)

val modules : Module.t list ref
val register : Module.t -> unit

val path_name : Binding.t -> Binding.t * Id.t
val mem_module : Binding.t -> bool
val find_module_opt : Binding.t -> Module.t option
val find_module : Binding.t -> Module.t
val find_tycon_opt : Binding.t -> Type_t.tycon option
val find_val_opt : Binding.t -> Type_t.t option
val find_val : Binding.t -> Type_t.t

val builtin_tycons : (Id.t * Type_t.tycon) list
