type t = Type_t.t

val newtyvar : unit -> Id.t
val newmetavar : unit -> t option ref

val to_string : t -> Id.t
val to_ocaml : t -> string

val equal : t -> t -> bool

val prefix : t -> Id.t

val app : Location.t -> Type_t.tycon -> t list -> t (* App *)
val void_app : Location.t -> Type_t.tycon -> t (* App (tycon, []) *)
val app_unit : Location.t -> t (* App (Unit, []) *)

val cons_id : Id.t
val nil_id : Id.t

module Tycon : sig

  type t = Type_t.tycon

  val to_string : t -> string
  val to_ocaml : t -> string

  val vars : t -> (Id.t * Type_t.t) list
  val types : t -> (Id.t * Type_t.t) list

end

module Constr : sig

  type t = Type_t.constr

  val to_string : t -> string

end
