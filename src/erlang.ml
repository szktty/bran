open Spotlib.Base

type closure = { entry : Id.l; actual_fv : Id.t list }
type t =
  | Atom of string
  | Int of IntRepr.t
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
  | PtInt of IntRepr.t
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

let true_atom = Atom "true"
let false_atom = Atom "false"
let ok_atom = Atom "ok"

let rec gen_exp (e, t) =
  Log.debug "# Erlang.gen_exp %s\n" (Closure.string_of_expr e);
  match e with
  | Closure.Bool true -> true_atom
  | Closure.Bool false -> false_atom
  | Closure.Int v -> Int v
  | Closure.String s -> String s
  | Closure.Atom s -> Atom s
  | Closure.Bitstring s -> Bitstring s
  | Closure.Tuple ets -> Tuple (List.map gen_exp ets)
  | Closure.Not et -> Not (gen_exp et)
  | Closure.And (e1, e2) -> And (gen_exp e1, gen_exp e2)
  | Closure.Or (e1, e2) -> Or (gen_exp e1, gen_exp e2)
  | Closure.Neg et -> Neg (gen_exp et)
  | Closure.Add (e1, e2) -> Add (gen_exp e1, gen_exp e2)
  | Closure.Sub (e1, e2) -> Sub (gen_exp e1, gen_exp e2)
  | Closure.Mul (e1, e2) -> Mul (gen_exp e1, gen_exp e2)
  | Closure.Div (e1, e2) -> Div (gen_exp e1, gen_exp e2)
  | Closure.Concat (e1, e2) -> Concat (gen_exp e1, gen_exp e2)
  | Closure.Eq (e1, e2) -> Eq (gen_exp e1, gen_exp e2)
  | Closure.LE (e1, e2) -> LE (gen_exp e1, gen_exp e2)
  | Closure.Var x -> Var x
  | Closure.AppDir (x, ets) -> AppDir (x, List.map gen_exp ets)
  | _ -> assert false

let rec gen_ptn = function
  | Closure.PtUnit -> PtAtom "ok"
  | Closure.PtBool v -> PtBool v
  | Closure.PtInt v -> PtInt v
  | Closure.PtVar (x, _) -> PtVar x
  | Closure.PtTuple ps -> PtTuple (List.map gen_ptn ps)
  | Closure.PtRecord xps ->
    PtRecord (List.map (fun (x, p) -> (x, gen_ptn p)) xps)
  | Closure.PtConstr (x, ps) -> assert false

and gen_term (term, t) =
  Log.debug "# Erlang.gen_term %s\n" (Closure.string_of_term term);
  match term with
  | Closure.Unit -> ok_atom
  | Closure.Exp e -> gen_exp e
  | Closure.If (e, tr1, tr2) ->
    If [(gen_exp e, gen_term tr1); (true_atom, gen_term tr2)]
  | Closure.Match (x, pts) ->
    Match (x, List.map (fun (p, t) -> gen_ptn p, gen_term t) pts)
  | Closure.Let ((x, _), e1, e2) ->
    Let (x, gen_term e1, gen_term e2)
  | _ -> assert false

let gen_def = function
  | Closure.TypeDef _ -> []
  | Closure.VarDef _ -> []
  | Closure.FunDef def ->
    [FunDef { name = def.name; args = def.args; formal_fv = def.formal_fv;
              body = gen_term def.body }]

let f (Closure.Prog defs) =
  Prog (List.concat & List.map gen_def defs)
