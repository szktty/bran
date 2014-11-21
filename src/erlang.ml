open Spotlib.Base

type closure = { entry : Id.l; actual_fv : Id.t list }
type t =
  | Atom of string
  | Bool of bool
  | Int of int
  | String of string
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
  | Eq of t * t
  | LE of t * t
  | Var of Id.t
  | AppCls of t * t list
  | AppDir of Id.l * t list
  | If of t * t * t
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

let literal_of_string s =
  (* adhoc *)
  "\"" ^ s ^ "\""

let literal_of_float f =
  (* adhoc *)
  Printf.sprintf "%f" f

let ok_atom = Atom "ok"

let gen_exp (term, t) =
  match term with
  | Closure.Unit -> ok_atom
  | _ -> ok_atom

let gen_def = function
  | Closure.TypeDef _ -> []
  | Closure.VarDef _ -> []
  | Closure.FunDef def ->
    [FunDef { name = def.name; args = def.args; formal_fv = def.formal_fv;
              body = gen_exp def.body }]

let f (Closure.Prog defs) =
  Prog (List.concat & List.map gen_def defs)
