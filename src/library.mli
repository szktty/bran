(* managing modules *)

val modules : Module.t list ref
val register : Module.t -> unit
val mem : Id.t -> bool
val find_opt : Id.t -> Module.t option
val find : Id.t -> Module.t
