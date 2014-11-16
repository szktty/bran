type t = ast Location.loc

and ast =
  | Nop (* semicolon, newline *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Not of t
  | Neg of t
  | FNeg of t
  | If of t * t * t
  | Bin of t * BinOp.t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | Local of Id.t * t (* Module.var, Module.(exp) *)
  | Def of fundef
  | App of t * t list
  | Tuple of t list
  | List of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
  | Typed of t * Type.t
  | SigDef of Id.t * Fun.t

and fundef = {
  name : Id.t * Type.t;
  rec_ : bool;
  args : FunArg.t list;
  body : t
}
