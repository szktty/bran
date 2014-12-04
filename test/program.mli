type beam = string

val prog : string
val flags : string list
val command : string list

val beam_path : string -> beam
val exec_path : string -> string

val compile : Sealing.Env.t -> string -> beam
val eval : Sealing.Env.t -> beam -> string -> Sealing.Result.t
val simple_test : Sealing.Env.t -> beam -> unit
