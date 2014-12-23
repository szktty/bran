type t = string (* 変数の名前 (caml2html: id_t) *)

type l = L of string (* top level functions *)

val pp_list : string list -> string

val counter : int ref
val genid : string -> string

val gentmp : string -> string

module Map : Xmap.S with type key = t
