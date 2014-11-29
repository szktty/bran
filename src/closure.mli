open Closure_t

val string_of_pattern : pattern -> string
val string_of_typed_expr : et -> string
val string_of_expr : expr -> string
val string_of_typed_term : t -> string
val string_of_term : term -> string
val string_of_def : def -> string
val fv : t -> S.t
val f : KNormal_t.def list -> prog
