exception Invalid_path

type t

val pervasives : t

val of_list : Id.t list -> t
val of_string : Id.t -> t

val path_name : t -> t option * Id.t
val path : t -> t option
val name : t -> Id.t

val to_string : t -> string
val to_list : t -> Id.t list
val to_erl_fun : t -> string
val to_erl_atom : t -> string
