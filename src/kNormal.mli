open KNormal_t

val f : Ast_t.def list -> def list
val string_of_typed_expr : et -> Id.t
val string_of_expr : expr -> Id.t
val string_of_typed_term : t -> Id.t
val string_of_term : term -> Id.t
val fold : ('a * 'b list -> 'c -> 'a * 'b list) -> 'a -> 'c list -> 'b list
val map : (Env.t -> def -> 'a) -> def list -> 'a list
