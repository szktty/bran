exception Unify of Type.t * Type.t
exception Error of Ast_t.expr Locating.t * Type.t * Type.t

val subst : Env.t -> Type.t M.t -> Type.t -> Type.t
val occur : Type.t option ref -> Type.t -> bool
val unify : Env.t -> Type.t -> Type.t -> unit
val test_unify : unit
val generalize : Env.t -> Type.t -> Type.t
val instantiate : Env.t -> Type.t -> Type.t
val deref_tycon : Env.t -> Type.tycon -> Type.tycon
val deref_type : Env.t -> Type.t -> Type.t
val deref_pattern : Env.t -> Ast_t.pattern -> Ast_t.pattern * Env.t
val deref_id_type : Env.t -> 'a * Type.t -> 'a * Type.t
val deref_typed_expr : Env.t -> Ast_t.t -> Ast_t.t
val deref_expr : Env.t -> Ast_t.expr -> Ast_t.expr
val deref_def : Env.t -> Ast_t.def -> Ast_t.def
val pattern : Env.t -> Ast_t.pattern -> Env.t * Type.t
val g : Env.t -> Ast_t.t -> Ast_t.expr * Type.t
val f' : Env.t -> Ast_t.t * Type.t -> Ast_t.expr * Type.t
val f : Ast_t.def list -> Ast_t.def list
