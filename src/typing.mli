exception Error of Ast.t * Type.t * Type.t
exception Top_level_error of Type.t

val extenv : Type.t M.t ref
val f : Ast.t -> Ast.t
