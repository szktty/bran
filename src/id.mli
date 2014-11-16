type t = string (* 変数の名前 (caml2html: id_t) *)

type l =
  | L of string (* top level functions *)
  | M of string (* module functions of Erlang *)

val wildcard : string

val pp_list : string list -> string

val counter : int ref
val genid : string -> string

val id_of_typ : Type.t -> string
val gentmp : Type.t -> string
