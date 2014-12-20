type t = Type_t.t

val newtyvar : unit -> Id.t
val newmetavar : unit -> t option ref

val to_string : t -> Id.t
val to_repr : t -> string

val equal : t -> t -> bool

val prefix : t -> Id.t

val app : Location.t -> Type_t.tycon -> t list -> t (* App *)
val void_app : Location.t -> Type_t.tycon -> t (* App (tycon, []) *)
val app_unit : Location.t -> t (* App (Unit, []) *)

module With : sig

  type _t = t
  type 'a t = {
    t : _t;
    desc : 'a;
  }

  val create : _t -> 'a -> 'a t

end

module Tycon : sig

  type t = Type_t.tycon

  val to_string : t -> string
  val to_repr : t -> string

  val vars : t -> (Id.t * Type_t.t) list
  val types : t -> (Id.t * Type_t.t) list

end

module Constr : sig

  type t = Type_t.constr

  val to_string : t -> string

end
