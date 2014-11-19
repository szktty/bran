type closure = { entry : Id.l; actual_fv : Id.t list }
type t = (* クロージャ変換後の式 (caml2html: closure_t) *)
    term * Type.t
and term =
  | Unit
  | WrapBody of Id.t * Type.t
  | UnwrapBody of Id.t * Type.t
  | Exp of et
  | If of et * t * t
  | Match of Id.t * (pattern * t) list
  | Let of (Id.t * Type.t) * t * t
  | MakeCls of (Id.t * Type.t) * closure * t
and et =
    expr * Type.t
and expr = 
  | Bool of bool
  | Int of int
  | Record of (Id.t * et) list
  | Field of et * Id.t
  | Tuple of et list
  | Not of et
  | And of et * et
  | Or of et * et
  | Neg of et
  | Add of et * et
  | Sub of et * et
  | Mul of et * et
  | Div of et * et
  | Eq of et * et
  | LE of et * et
  | Var of Id.t
  | Constr of Id.t * et list
  | AppCls of et * et list
  | AppDir of Id.l * et list
and pattern =
  | PtBool of bool
  | PtInt of int
  | PtVar of Id.t * Type.t
  | PtTuple of pattern list
  | PtRecord of (Id.t * pattern) list
  | PtConstr of Id.t * pattern list
type fundef = {
  name : Id.l * Type.t;
  args : (Id.t * Type.t) list;
  formal_fv : (Id.t * Type.t) list;
  body : t;
}
and def =
  | TypeDef of Id.t * Type.tycon
  | VarDef of (Id.t * Type.t) * t
  | FunDef of fundef
type prog = Prog of def list

val string_of_pattern : pattern -> string
val string_of_typed_expr : et -> string
val string_of_expr : expr -> string
val string_of_typed_term : t -> string
val string_of_term : term -> string
val string_of_def : def -> string
val fv : t -> S.t
val f : KNormal.def list -> prog
