open Ast_t

val string_of_pattern : pattern -> string
val string_of_typed_expr : t -> string
val string_of_expr : expr -> string
val string_of_fundef : fundef -> string
val string_of_sigdef : sigdef -> string
val string_of_def : def -> string
val fold : (Env.t * 'a list -> def -> 'a list) -> def list -> Env.t -> 'a list
