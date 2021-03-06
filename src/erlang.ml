open Spotlib.Base
open Erlang_t

let literal_of_string s =
  (* adhoc *)
  "\"" ^ s ^ "\""

let literal_of_float f =
  (* adhoc *)
  Printf.sprintf "%f" f

let true_atom = Atom "true"
let false_atom = Atom "false"
(* let ok_atom = Atom "ok" *)

let rec gen_exp (e, t) =
  Log.debug "# Erlang.gen_exp %s\n" (Closure.string_of_expr e);
  match e with
  | Closure_t.Bool true -> true_atom
  | Closure_t.Bool false -> false_atom
  | Closure_t.Int v -> Int v
  | Closure_t.Float v -> Float v
  | Closure_t.Char s -> Char s
  | Closure_t.String s -> String s
  | Closure_t.Atom s -> Atom s
  | Closure_t.Bitstring s -> Bitstring s
  | Closure_t.Tuple ets -> Tuple (List.map gen_exp ets)
  | Closure_t.List ets -> List (List.map gen_exp ets)
  | Closure_t.Array ets -> Array (List.map gen_exp ets)
  | Closure_t.Not et -> Not (gen_exp et)
  | Closure_t.And (e1, e2) -> And (gen_exp e1, gen_exp e2)
  | Closure_t.Or (e1, e2) -> Or (gen_exp e1, gen_exp e2)
  | Closure_t.Neg et -> Neg (gen_exp et)
  | Closure_t.Add (e1, e2) -> Add (gen_exp e1, gen_exp e2)
  | Closure_t.Sub (e1, e2) -> Sub (gen_exp e1, gen_exp e2)
  | Closure_t.Mul (e1, e2) -> Mul (gen_exp e1, gen_exp e2)
  | Closure_t.Div (e1, e2) -> Div (gen_exp e1, gen_exp e2)
  | Closure_t.Concat (e1, e2) -> Concat (gen_exp e1, gen_exp e2)
  | Closure_t.Eq (e1, e2) -> Eq (gen_exp e1, gen_exp e2)
  | Closure_t.LE (e1, e2) -> LE (gen_exp e1, gen_exp e2)
  | Closure_t.Var x -> Var x
  | Closure_t.AppDir (x, ets) -> AppDir (x, List.map gen_exp ets)
  | Closure_t.Constr (x, ets) -> Constr (x, List.map gen_exp ets)
  | _ -> failwith & "not implemented: " ^ (Closure.string_of_expr e)

let rec gen_ptn = function
  | Closure_t.PtUnit -> PtTuple []
  | Closure_t.PtBool v -> PtBool v
  | Closure_t.PtInt v -> PtInt v
  | Closure_t.PtFloat v -> PtFloat v
  | Closure_t.PtAtom v -> PtAtom v
  | Closure_t.PtString v -> PtString v
  | Closure_t.PtVar (x, _) -> PtVar x
  | Closure_t.PtAlias (p, x, _) -> PtAlias (gen_ptn p, x)
  | Closure_t.PtTuple ps -> PtTuple (List.map gen_ptn ps)
  | Closure_t.PtList ps -> PtList (List.map gen_ptn ps)
  | Closure_t.PtCons (p1, p2) -> PtCons (gen_ptn p1, gen_ptn p2)
  | Closure_t.PtRecord xps ->
    PtRecord (List.map (fun (x, p) -> (x, gen_ptn p)) xps)
  | Closure_t.PtConstr (x, ps) -> PtConstr (x, List.map gen_ptn ps)

and gen_term (term, t) =
  Log.debug "# Erlang.gen_term %s\n" (Closure.string_of_term term);
  match term with
  | Closure_t.Unit -> Tuple []
  | Closure_t.Exp e -> gen_exp e
  | Closure_t.If (e, tr1, tr2) ->
    If [(gen_exp e, gen_term tr1); (true_atom, gen_term tr2)]
  | Closure_t.Match (x, pts) ->
    Match (x, List.map (fun (p, t) -> gen_ptn p, gen_term t) pts)
  | Closure_t.Let ((x, _), e1, e2) ->
    Let (x, gen_term e1, gen_term e2)
  | _ -> assert false

let gen_def = function
  | Closure_t.TypeDef (x, tycon) -> [TypeDef (x, tycon)]
  | Closure_t.VarDef _ -> []
  | Closure_t.FunDef def ->
    [FunDef { name = def.name; args = def.args; formal_fv = def.formal_fv;
              body = gen_term def.body }]

let f (Closure_t.Prog defs) =
  Prog (List.concat & List.map gen_def defs)
