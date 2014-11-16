type t = Type.fun_

val create : Type.t list -> Type.t -> t

val to_string : t -> string
val to_typ : t -> Type.t

val erl_sig : t -> string
