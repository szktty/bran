type t = term * Type_t.t

and term =
  | Unit
  | Exp of et
  | If of et * t * t 
  | Match of Id.t * (pattern * t) list
  | Let of (Id.t * Type_t.t) * t * t
  | LetRec of fundef * t

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
  | List of et list
  | Tuple of et list
  | Array of et list
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
  | Var of [`Local of Id.t | `Module of Binding.t]
  | Concat of et * et
  | Constr of Binding.t * et list
  | App of et * et list
  | ExtFunApp of Id.t * et list
  | Get of et * et
  | Put of et * et * et

and pattern =
  | PtUnit
  | PtBool of bool
  | PtInt of IntRepr.t
  | PtAtom of string
  | PtString of string
  | PtVar of Id.t * Type_t.t
  | PtTuple of pattern list
  | PtField of (Id.t * pattern) list
  | PtConstr of Id.t * pattern list

and fundef = {
  name : Id.t * Type_t.t;
  args : (Id.t * Type_t.t) list;
  body : t;
}

and def =
  | TypeDef of (Id.t * Type_t.tycon)
  | VarDef of (Id.t * Type_t.t) * t
  | RecDef of fundef
