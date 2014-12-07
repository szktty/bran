exception Invalid_path

type t = Id.t list * Id.t

val of_list : Id.t list -> t
val of_string : Id.t -> t

val to_string : t -> string
val to_list : t -> Id.t list
