type t = term * Type.t

and term =
  | Unit
  | Exp of et
  | If of et * t * t 
  | Match of Id.t * (pattern * t) list
  | Let of (Id.t * Type.t) * t * t
  | LetRec of fundef * t

and et = expr * Type.t

and expr =
  | Bool of bool
  | Int of IntRepr.t
  | Char of string
  | String of string
  | Atom of string
  | Bitstring of Bitstring.t
  | Record of (Id.t * et) list
  | Field of et * Id.t
  | Module of Id.t
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
  | Concat of et * et
  | Constr of Id.t * et list
  | App of et * et list
  | ExtFunApp of Id.t * et list

and pattern =
  | PtUnit
  | PtBool of bool
  | PtInt of IntRepr.t
  | PtVar of Id.t * Type.t
  | PtTuple of pattern list
  | PtField of (Id.t * pattern) list
  | PtConstr of Id.t * pattern list

and fundef = {
  name : Id.t * Type.t;
  args : (Id.t * Type.t) list;
  body : t;
}

and def =
  | TypeDef of (Id.t * Type.tycon)
  | VarDef of (Id.t * Type.t) * t
  | RecDef of fundef