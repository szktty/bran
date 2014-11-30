type closure = {
  entry : Id.l;
  actual_fv : Id.t list;
}

type t =
  | Atom of string
  | Int of IntRepr.t
  | Float of float
  | Char of string
  | String of string
  | Bitstring of Bitstring.t
  | Record of (Id.t * t) list
  | Field of t * Id.t
  | List of t list
  | Array of t list
  | Tuple of t list
  | Not of t
  | And of t * t
  | Or of t * t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Concat of t * t
  | Eq of t * t
  | LE of t * t
  | Var of Id.t
  | AppCls of t * t list
  | AppDir of Id.l * t list
  | If of (t * t) list
  | Match of Id.t * (pattern * t) list
  | Let of Id.t * t * t
  | MakeCls of Id.t * closure * t

and pattern =
  | PtAtom of string
  | PtBool of bool
  | PtInt of IntRepr.t
  | PtString of string
  | PtVar of Id.t
  | PtList of pattern list
  | PtTuple of pattern list
  | PtRecord of (Id.t * pattern) list

type fundef = {
  name : Id.l * Type_t.t;
  args : (Id.t * Type_t.t) list;
  formal_fv : (Id.t * Type_t.t) list;
  body : t;
}

and def =
  | ModuleDef of Id.t
  | Export of Id.t list
  | TypeDef of Id.t * Type_t.tycon
  | VarDef of (Id.t * Type_t.t) * t
  | FunDef of fundef

type prog = Prog of def list
