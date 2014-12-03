type t = Type_t.t

val newtyvar : unit -> Id.t
val newmetavar : unit -> t option ref

val to_string : t -> Id.t
val to_ocaml : t -> string

val equal : t -> t -> bool

val prefix : t -> Id.t
val name : t -> Id.t

val app_unit : t (* App (Unit, []) *)

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
