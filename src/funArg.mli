type t = 
  | Var of (Id.t * Type.t)
  | Tuple of Id.t * t list

val typ : t -> Type.t
val names : t list -> Id.t list
val vars : t list -> (Id.t * Type.t) list

