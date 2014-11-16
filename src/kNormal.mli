type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | SConcat of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | Mod_fun of Fun.t
  (*| LetRec of fundef * t*)
  | Def of fundef
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | List of Id.t list
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
and fundef = {
  name : Id.t * Type.t;
  rec_ : bool;
  args : FunArg.t list;
  body : t
}

val to_string : t -> string

val fv : t -> S.t
val f : AstTypes.t -> t

