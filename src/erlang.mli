open Erlang_t

val literal_of_string : string -> string
val literal_of_float : float -> string

val f : Closure_t.prog -> prog
