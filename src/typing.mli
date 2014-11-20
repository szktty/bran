exception Unify of Type.t * Type.t
exception Error of Syntax.expr Locating.t * Type.t * Type.t
val subst : Env.t -> Type.t M.t -> Type.t -> Type.t
val occur : Type.t option ref -> Type.t -> bool
val unify : Env.t -> Type.t -> Type.t -> unit
val test_unify : unit
val generalize : Env.t -> Type.t -> Type.t
val instantiate : Env.t -> Type.t -> Type.t
val deref_tycon : Env.t -> Type.tycon -> Type.tycon
val deref_type : Env.t -> Type.t -> Type.t
val deref_pattern : Env.t -> Syntax.pattern -> Syntax.pattern * Env.t
val deref_id_type : Env.t -> 'a * Type.t -> 'a * Type.t
val deref_typed_expr : Env.t -> Syntax.t -> Syntax.t
val deref_expr : Env.t -> Syntax.expr -> Syntax.expr
val deref_def : Env.t -> Syntax.def -> Syntax.def
val pattern : Env.t -> Syntax.pattern -> Env.t * Type.t
val g : Env.t -> Syntax.t -> Syntax.expr * Type.t
val f' : Env.t -> Syntax.t * Type.t -> Syntax.expr * Type.t
val f : Syntax.def list -> Syntax.def list
