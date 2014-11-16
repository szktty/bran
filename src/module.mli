type t = Type.module_

val create : ?erl:string -> string -> (string * Fun.t) list -> t

val of_typ : Type.t -> t

val find_opt : t -> string -> Fun.t option
val find : t -> string -> Fun.t
                            
val add_fun : t -> string -> Fun.t -> unit
val fun_typs : t -> (string * Type.t) list
