exception Syntax_error of Location.t * string option
exception Unbound_value_error of Location.t * Id.t
exception Unbound_module_error of Location.t * Id.t

type t = (expr * Type_t.t) With.Loc.t
and expr =
    Unit
  | Bool of bool
  | Int of IntRepr.t
  | Float of float
  | Char of string
  | String of string
  | Atom of string
  | Bitstring of Bitstring.t
  | Record of (Id.t * t) list
  | Field of t * Id.t
  | List of t list
  | Tuple of t list
  | Array of t list
  | Not of t
  | And of t * t
  | Or of t * t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Match of t * (pattern * t) list
  | LetVar of (Id.t * Type_t.t) * t * t
  | Var of [`Unbound of Binding.t | `Local of Id.t | `Module of Binding.t]
  | Concat of t * t
  | Constr of Binding.t * t list
  | LetRec of fundef * t
  | App of t * t list
  | Get of t * t
  | Put of t * t * t
  | Perform of t
  | Bind of (Id.t * Type_t.t) * t
  | Return of t
and pattern = pattern_desc With.Loc.t
and pattern_desc =
  | PtUnit
  | PtBool of bool
  | PtInt of IntRepr.t
  | PtFloat of float
  | PtAtom of string
  | PtString of string
  | PtVar of Id.t * Type_t.t
  | PtAlias of pattern * Id.t * Type_t.t
  | PtTuple of pattern list
  | PtList of pattern list
  | PtCons of pattern * pattern
  | PtRecord of (Id.t * pattern) list
  | PtConstr of Binding.t * pattern list * Type_t.t
and fundef = { name : Id.t * Type_t.t; args : (Id.t * Type_t.t) list; body : t; }
and sigdef = {
  sig_name : Id.t * Type_t.t;
  sig_ext : string option;
}
and def = def_desc With.Loc.t
and def_desc =
  | Nop
  | TypeDef of Id.t * Type_t.tycon
  | VarDef of (Id.t * Type_t.t) * t
  | RecDef of fundef
  | SigDef of sigdef

