type closure = {
  entry : Id.l;
  actual_fv : Id.t list;
}

type t = term * Type_t.t

and term =
  | Unit
  | Exp of et
  | If of et * t * t
  | Match of Id.t * (pattern * t) list
  | Let of (Id.t * Type_t.t) * t * t
  | MakeCls of (Id.t * Type_t.t) * closure * t

and et = expr * Type_t.t

and expr = 
  | Bool of bool
  | Int of IntRepr.t
  | Float of float
  | Char of string
  | String of string
  | Atom of string
  | Bitstring of Bitstring.t
  | Record of (Id.t * et) list
  | Field of et * Id.t
  | Tuple of et list
  | List of et list
  | Array of et list
  | Not of et
  | And of et * et
  | Or of et * et
  | Neg of et
  | Add of et * et
  | Sub of et * et
  | Mul of et * et
  | Div of et * et
  | Concat of et * et
  | Eq of et * et
  | LE of et * et
  | Var of Binding.t
  | Constr of Binding.t * et list
  | AppCls of et * et list
  | AppDir of Binding.t * et list
  | Get of et * et
  | Put of et * et * et

and pattern =
  | PtUnit
  | PtBool of bool
  | PtInt of IntRepr.t
  | PtVar of Id.t * Type_t.t
  | PtTuple of pattern list
  | PtRecord of (Id.t * pattern) list
  | PtConstr of Id.t * pattern list

type fundef = {
  name : Id.l * Type_t.t;
  args : (Id.t * Type_t.t) list;
  formal_fv : (Id.t * Type_t.t) list;
  body : t;
}

and def =
  | TypeDef of Id.t * Type_t.tycon
  | VarDef of (Id.t * Type_t.t) * t
  | FunDef of fundef

type prog = Prog of def list
