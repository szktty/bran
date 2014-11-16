(* signature file *)

exception Error of Location.t * string

val load : string -> Ast.t -> unit
