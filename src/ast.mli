open Ast_t

val string_of_typed_expr : t -> string
val string_of_expr : expr -> string
val string_of_fundef : fundef -> string
val string_of_sigdef : sigdef -> string
val string_of_def : def -> string
val fold : (Env.t * 'a list -> def -> 'a list) -> def list -> Env.t -> 'a list

module Pattern : sig

  type t = pattern

  val to_string : t -> string

  val fold :
    ('b list -> 'c)
    -> ('env -> 'a -> 'env * 'b)
    -> 'env
    -> 'a list
    -> 'env * 'c

  val fold_bin :
    ('b -> 'b -> 'c)
    -> ('env -> 'a -> 'env * 'b)
    -> 'env
    -> 'a
    -> 'a
    -> 'env * 'c

  val fold_assoc :
    (('x * 'b) list -> 'c)
    -> ('env -> 'a -> 'env * 'b)
    -> 'env
    -> ('x * 'a) list
    -> 'env * 'c

end
