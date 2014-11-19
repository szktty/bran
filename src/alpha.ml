(* rename identifiers to make them unique (alpha-conversion) *)

open KNormal

let find x ids = try M.find x ids with Not_found -> x
let genid x ids = if (M.mem x ids) then Id.genid x else x
let add x y ids = M.add x y ids
let add_list xs ids = List.fold_left (fun ids x -> add x (genid x ids) ids) ids xs

let rec h ids (e, t) =
  let e' = 
    match e with
    | Bool(b) -> Bool(b)
    | Int(i) -> Int(i)
    | Record(xes) -> Record(List.map (fun (x, e) -> find x ids, (h ids e)) xes)
    | Field(e, x) -> Field(h ids e, find x ids)
    | Tuple(es) -> Tuple(List.map (h ids) es)
    | Var(x) -> Var(find x ids)
    | Constr(x, es) -> Constr(find x ids, List.map (h ids) es)
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
    | ExtFunApp(x, ys) -> ExtFunApp(x, List.map (h ids) ys) in
  (e', t)

let rec pattern ids =
  function
  | PtBool(b) -> ids, (PtBool(b))
  | PtInt(n) -> ids, (PtInt(n))
  | PtVar(x, t) -> let x' = genid x ids in (add x x' ids), (PtVar(x', t)) 
  | PtTuple(ps) -> 
      let ids', ps' = 
        List.fold_left 
          (fun (ids, ps) p -> 
            let ids', p = pattern ids p in
            (ids', p :: ps))
          (ids, []) (List.rev ps) in
      ids', PtTuple(ps')
  | PtField(xps) -> 
      let ids', xps' = 
        List.fold_left 
          (fun (ids, xps) (x, p) -> 
            let ids', p = pattern ids p in
            (ids', (x, p) :: xps))
          (ids, []) (List.rev xps) in
      ids', PtField(xps')
  | PtConstr(x, ps) -> 
      let ids', ps' = 
        List.fold_left 
          (fun (ids, ps) p -> 
            let ids', p = pattern ids p in
            (ids', p :: ps))
          (ids, []) (List.rev ps) in
      ids', PtConstr(x, ps')

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
    | WrapBody(x, t) -> WrapBody(x, t)
    | UnwrapBody(x, t) -> UnwrapBody(x, t) in
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
  fold f' M.empty
