val init_module : string -> unit
val finish_module : unit -> unit

val modules : unit -> Module.t list
val add_module : Module.t -> unit
val find_module_opt : string -> Module.t option
val find_module : string -> Module.t
val current_module : unit -> Module.t option

val top_funs : unit -> (Id.t * Id.t) list
val add_top_fun : Id.t -> Id.t -> unit

val add_top_typ : Id.t -> Fun.t -> unit
val mem_top_typ : Id.t -> bool
val find_top_typ : Id.t -> Fun.t
