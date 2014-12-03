type beam = string

val prog : string
val flags : string list
val command : string list

val beam_path : string -> beam
val exec_path : string -> string

val compile : Sealing.Env.t -> string -> beam
val call_main : Sealing.Env.t -> beam -> Sealing.Result.t
