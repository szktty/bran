open Type_t

val newtyvar : unit -> Id.t
val newmetavar : unit -> t option ref

val string_of_t : t -> Id.t
val string_of_tycon : tycon -> Id.t
val string_of_constr : constr -> string
val ocaml_of : t -> Id.t
val ocaml_of_tycon : tycon -> Id.t

val equal : t -> t -> bool
val vars : tycon -> (Id.t * t) list
val types : tycon -> (Id.t * t) list

val prefix : t -> Id.t
val name : t -> Id.t

val app_unit : t (* App (Unit, []) *)
