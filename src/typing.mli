exception Unify of Type_t.t * Type_t.t
exception Topdef_error of (Id.t * Type_t.t) * Type_t.t * Type_t.t
exception Error of Ast_t.expr Locating.t * Type_t.t * Type_t.t

val subst : Env.t -> Type_t.t M.t -> Type_t.t -> Type_t.t
val occur : Type_t.t option ref -> Type_t.t -> bool
val unify : Env.t -> Type_t.t -> Type_t.t -> unit
val test_unify : unit
val generalize : Env.t -> Type_t.t -> Type_t.t
val instantiate : Env.t -> Type_t.t -> Type_t.t
val deref_tycon : Env.t -> Type_t.tycon -> Type_t.tycon
val deref_type : Env.t -> Type_t.t -> Type_t.t
val deref_pattern : Env.t -> Ast_t.pattern -> Ast_t.pattern * Env.t
val deref_id_type : Env.t -> 'a * Type_t.t -> 'a * Type_t.t
val deref_typed_expr : Env.t -> Ast_t.t -> Ast_t.t
val deref_expr : Env.t -> Ast_t.expr -> Ast_t.expr
val deref_def : Env.t -> Ast_t.def -> Ast_t.def
val pattern : Env.t -> Ast_t.pattern -> Env.t * Type_t.t
val g : Env.t -> Ast_t.t -> Ast_t.expr * Type_t.t
val f' : Env.t -> Ast_t.t * Type_t.t -> Ast_t.expr * Type_t.t
val f : Ast_t.def list -> Ast_t.def list
