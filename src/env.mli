type t = Type.t M.t

val create : unit -> t

val import : t -> Module.t -> t
