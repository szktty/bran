(* signature file *)

exception Error of Location.t * string

val load : Id.t -> Syntax.def list -> unit
