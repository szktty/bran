(* rename identifiers to make them unique (alpha-conversion) *)

open KNormal_t
open Base

let find x ids = try M.find x ids with Not_found -> x
let genid x ids = if (M.mem x ids) then Id.genid x else x
let add x y ids = M.add x y ids
let add_list xs ids = List.fold_left (fun ids x -> add x (genid x ids) ids) ids xs

let rec h ids (e, t) =
  let e' = 
    match e with
    | Bool(b) -> Bool(b)
    | Int(i) -> Int(i)
    | Float v -> Float v
    | Char s -> Char s
    | String s -> String s
    | Atom s -> Atom s
    | Bitstring s -> Bitstring s
    | Record(xes) -> Record(List.map (fun (x, e) -> find x ids, (h ids e)) xes)
    | Field(e, x) -> Field(h ids e, find x ids)
    | Tuple(es) -> Tuple(List.map (h ids) es)
    | List(es) -> List(List.map (h ids) es)
    | Array(es) -> Array(List.map (h ids) es)
    | Var(`Local x) -> Var(`Local (find x ids))
    | Var(`Module x) -> Var(`Module x)
    | Concat(e1, e2) -> Concat(h ids e1, h ids e2)
    | Constr(x, es) ->
      Constr(Binding.of_string & find (Binding.to_string x) ids,
             List.map (h ids) es)
    | Not(e) -> Not(h ids e)
    | And(e1, e2) -> And(h ids e1, h ids e2)
    | Or(e1, e2) -> Or(h ids e1, h ids e2)
    | Neg(e) -> Neg(h ids e)
    | Add(e1, e2) -> Add(h ids e1, h ids e2)
    | Sub(e1, e2) -> Sub(h ids e1, h ids e2)
    | Mul(e1, e2) -> Mul(h ids e1, h ids e2)
    | Div(e1, e2) -> Div(h ids e1, h ids e2)
    | Eq(e1, e2) -> Eq(h ids e1, h ids e2)
    | LE(e1, e2) -> LE(h ids e1, h ids e2)
    | App(e, ys) -> App(h ids e, List.map (h ids) ys)
    | ExtFunApp(x, ys) -> ExtFunApp(x, List.map (h ids) ys)
    | Get(e1, e2) -> Get(h ids e1, h ids e2)
    | Put(e1, e2, e3) -> Put(h ids e1, h ids e2, h ids e3)
  in
  (e', t)

let rec pattern ids p =
  let open Ast.Pattern in
  match p with
  | PtUnit -> ids, PtUnit
  | PtBool(b) -> ids, (PtBool(b))
  | PtInt(n) -> ids, (PtInt(n))
  | PtFloat v -> ids, PtFloat v
  | PtAtom v -> ids, PtAtom v
  | PtString v -> ids, PtString v
  | PtVar(x, t) -> let x' = genid x ids in (add x x' ids), (PtVar(x', t)) 
  | PtTuple ps -> fold (fun ps' -> PtTuple ps') pattern ids ps
  | PtList ps -> fold (fun ps' -> PtList ps') pattern ids ps
  | PtCons (p1, p2) ->
    fold_bin (fun p1' p2' -> PtCons (p1', p2')) pattern ids p1 p2
  | PtField(xps) -> 
    fold_assoc (fun xps' -> PtField xps') pattern ids xps
  | PtConstr(x, ps) -> 
    fold (fun ps' -> PtConstr (x, ps')) pattern ids ps

let rec g ids (e, t) = (* α変換ルーチン本体 (caml2html: alpha_g) *)
  let e' = 
    match e with
    | Unit -> Unit
    | Exp(e) -> Exp(h ids e)
    | If(e, e1, e2) -> If(h ids e, g ids e1, g ids e2)
    | Match(x, pes) -> Match(find x ids, List.map (fun (p, e) -> let ids', p' = pattern ids p in p', g ids' e) pes)
    | Let((x, t), e1, e2) -> (* letのα変換 (caml2html: alpha_let) *)
        let x' = genid x ids in
        Let((x', t), g ids e1, g (add x x' ids) e2)
    | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let recのα変換 (caml2html: alpha_letrec) *)
        let ids = add x (genid x ids) ids in
        let ys = List.map fst yts in
        let ids' = add_list ys ids in
        LetRec({ name = (find x ids, t);
	         args = List.map (fun (y, t) -> (find y ids', t)) yts;
	         body = g ids' e1 },
	       g ids e2)
  in
  (e', t)
      
let f =
  let f' (ids, defs) =
    function
    | TypeDef(x, t) -> 
        (add x (genid x ids) ids), TypeDef(x, t) :: defs
    | VarDef((x, t), e) -> 
        (add x (genid x ids) ids), VarDef((x, t), g ids e) :: defs
    | RecDef({ name = (x, t); args = yts; body = e1 }) -> 
        let ids = add x (genid x ids) ids in
        let ys = List.map fst yts in
        let ids' = add_list ys ids in
        ids, RecDef({ name = (x, t); args = List.map (fun (y, t) -> (find y ids', t)) yts; body = g ids' e1 }) :: defs in
  KNormal.fold f' M.empty
