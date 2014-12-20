type t = Type_t.t

val newtyvar : unit -> Id.t

val to_string : t -> Id.t
val to_repr : t -> string

val equal : t -> t -> bool

val prefix : t -> Id.t

val app : Location.t -> Type_t.tycon -> t list -> t (* App *)
val void_app : Location.t -> Type_t.tycon -> t (* App (tycon, []) *)
val app_unit : Location.t -> t (* App (Unit, []) *)

module With : sig

  type with_ = t

  type 'a t = {
    with_ : with_;
    desc : 'a;
  }

  val create : with_ -> 'a -> 'a t

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

module Meta : sig

  val create : Location.t -> t

end
