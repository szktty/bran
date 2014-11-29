module Bits : sig

  type t = {
    value : value;
    size : int option;
    typ : typ;
    sign : sign option;
    endian : endian option;
    unit : int option;
  }

  and value =
    | Int of int
    | Float of float
    | String of string
    | Var of Id.t

  and typ = [`Int | `Float | `Binary | `Bitstring | `UTF8 | `UTF16 | `UTF32]
  and sign = [`Signed | `Unsigned]
  and endian = [`Big | `Little | `Native]

  val create :
    ?size:int
    -> ?typ:typ
    -> ?sign:sign
    -> ?endian:endian
    -> ?unit:int
    -> value
    -> t

  val validate : t -> t

  val to_string : t -> string

end


type t = Bits.t list

val create : Bits.t list -> t
val to_string : t -> string
val length : t -> int
