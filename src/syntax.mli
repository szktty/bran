exception Syntax_error of Location.t * string option
exception Unbound_value_error of Location.t * Id.t
exception Unbound_module_error of Location.t * Id.t

type t = (expr * Type.t) Locating.t
and expr =
    Unit
  | Bool of bool
  | Int of int
  | String of string
  | Atom of string
  | Bitstring of Bitstring.t
  | Record of (Id.t * t) list
  | Field of t * Id.t
  | Tuple of t list
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
  | LetVar of (Id.t * Type.t) * t * t
  | Var of Id.t
  | Concat of t * t
  | Constr of Id.t * t list
  | Module of Id.t
  | LetRec of fundef * t
  | App of t * t list
and pattern = pattern_desc Locating.t
and pattern_desc =
  | PtUnit
  | PtBool of bool
  | PtInt of int
  | PtVar of Id.t * Type.t
  | PtTuple of pattern list
  | PtRecord of (Id.t * pattern) list
  | PtConstr of Id.t * pattern list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t; }
and sigdef = {
  sig_name : Id.t * Type.t;
  sig_ext : string option;
}
and def = def_desc Locating.t
and def_desc =
  | Nop
  | TypeDef of Id.t * Type.tycon
  | VarDef of (Id.t * Type.t) * t
  | RecDef of fundef
  | SigDef of sigdef
val string_of_pattern : pattern -> string
val string_of_typed_expr : t -> string
val string_of_expr : expr -> string
val string_of_fundef : fundef -> string
val string_of_sigdef : sigdef -> string
val string_of_def : def -> string
val fold : (Env.t * 'a list -> def -> 'a list) -> def list -> Env.t -> 'a list
