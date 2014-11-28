(* convert Closure.t to Erlang syntax *)
type closure = { entry : Id.l; actual_fv : Id.t list }
type t =
  | Atom of string
  | Int of int
  | String of string
  | Bitstring of Bitstring.t
  | Record of (Id.t * t) list
  | Field of t * Id.t
  | List of t list
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
  | PtInt of int
  | PtString of string
  | PtVar of Id.t
  | PtList of pattern list
  | PtTuple of pattern list
  | PtRecord of (Id.t * pattern) list
type fundef = {
  name : Id.l * Type.t;
  args : (Id.t * Type.t) list;
  formal_fv : (Id.t * Type.t) list;
  body : t;
}
and def =
  | ModuleDef of Id.t
  | Export of Id.t list
  | TypeDef of Id.t * Type.tycon
  | VarDef of (Id.t * Type.t) * t
  | FunDef of fundef
type prog = Prog of def list

val literal_of_string : string -> string
val literal_of_float : float -> string

val f : Closure.prog -> prog
