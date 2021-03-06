open Base
open Closure_t

let rec string_of_pattern =
  function
  | PtUnit -> "PtUnit"
  | PtBool(b) -> "PtBool(" ^ (string_of_bool b) ^ ")"
  | PtInt(n) -> "PtInt(" ^ (IntRepr.to_string n) ^ ")"
  | PtFloat v -> "PtFloat(" ^ (string_of_float v) ^ ")"
  | PtAtom v -> "PtAtom(" ^ v ^ ")"
  | PtString v -> "PtString(" ^ v ^ ")"
  | PtVar(x, t) -> "PtVar(" ^ x ^ "," ^ (Type.to_string t) ^ ")"
  | PtAlias (p, x, t) ->
    Printf.sprintf "PtAlias(%s, %s, %s)" (string_of_pattern p) x (Type.to_string t)
  | PtTuple(ps) -> "PtTuple(" ^ (String.concat_map "; " string_of_pattern ps) ^ ")"
  | PtList(ps) -> "PtList(" ^ (String.concat_map "; " string_of_pattern ps) ^ ")"
  | PtCons (p1, p2) ->
    Printf.sprintf "PtCons(%s)" (String.concat_map ", " string_of_pattern [p1; p2])
  | PtRecord(xps) -> "PtRecord([" ^ (String.concat "; " (List.map (fun (x, p) -> x ^ ", " ^ (string_of_pattern p)) xps)) ^ "])"
  | PtConstr(x, ps) ->
    "PtConstr(" ^ (Binding.to_string x) ^ ", [" ^
    (String.concat_map "; " string_of_pattern ps) ^ "])"

let rec string_of_typed_expr (e, t) = (string_of_expr e) ^ " : " ^ (Type.to_string t)

and string_of_expr = 
  function
  | Bool(b) -> string_of_bool b
  | Int(n) -> IntRepr.to_string n
  | Float(v) -> string_of_float v
  | Char(s) -> "'" ^ s ^ "'"
  | String(s) -> "\"" ^ s ^ "\""
  | Atom(s) -> "@\"" ^ s ^ "\""
  | Bitstring x -> Bitstring.to_string x
  | Record(xes) -> "{" ^ (String.concat "; " (List.map (fun (x, e) -> x ^ " = " ^ (string_of_typed_expr e)) xes)) ^ "}"
  | Field(e, x) -> (string_of_typed_expr e) ^ "." ^ x
  | Tuple(es) -> "(" ^ (String.concat_map ", " string_of_typed_expr es) ^ ")"
  | List(es) -> "[" ^ (String.concat_map ", " string_of_typed_expr es) ^ "]"
  | Array(es) -> "[|" ^ (String.concat_map "; " string_of_typed_expr es) ^ "|]"
  | Not(e) -> "not " ^ (string_of_typed_expr e)
  | And(e1, e2) -> (string_of_typed_expr e1) ^ " && " ^ (string_of_typed_expr e2)
  | Or(e1, e2) -> (string_of_typed_expr e1) ^ " || " ^ (string_of_typed_expr e2)
  | Neg(e) -> "-" ^ (string_of_typed_expr e)
  | Add(e1, e2) -> (string_of_typed_expr e1) ^ " + " ^ (string_of_typed_expr e2)
  | Sub(e1, e2) -> (string_of_typed_expr e1) ^ " - " ^ (string_of_typed_expr e2)
  | Mul(e1, e2) -> (string_of_typed_expr e1) ^ " * " ^ (string_of_typed_expr e2)
  | Div(e1, e2) -> (string_of_typed_expr e1) ^ " / " ^ (string_of_typed_expr e2)
  | Concat (e1, e2) -> (string_of_typed_expr e1) ^ " ^ " ^ (string_of_typed_expr e2)
  | Eq(e1, e2) -> (string_of_typed_expr e1) ^ " = " ^ (string_of_typed_expr e2)
  | LE(e1, e2) -> (string_of_typed_expr e1) ^ " <= " ^ (string_of_typed_expr e2) 
  | Var(`Local x) -> "Var(`Local " ^ x ^ ")"
  | Var(`Module x) -> "Var(`Module " ^ (Binding.to_string x) ^ ")"
  | Constr(x, es) ->
    "Constr(" ^ (Binding.to_string x) ^ ", [" ^ (String.concat_map "; " string_of_typed_expr es) ^ "])"
  | AppCls(e, args) -> "AppCls(" ^ (string_of_typed_expr e) ^ ", [" ^ (String.concat "; " (List.map string_of_typed_expr args)) ^ "])"
  | AppDir(x, args) ->
    "AppDir(" ^ (Binding.to_string x) ^ ", [" ^ (String.concat_map " " string_of_typed_expr args) ^ "])"
  | Get(e1, e2) -> "Get(" ^ (string_of_typed_expr e1) ^ ", " ^ (string_of_typed_expr e2)
  | Put(e1, e2, e3) -> "Put(" ^ (string_of_typed_expr e1) ^ ", " ^ (string_of_typed_expr e2) ^ ", " ^ (string_of_typed_expr e3)
      
let rec string_of_typed_term (e, t) = (string_of_term e) ^ " : " ^ (Type.to_string t)

and string_of_term = 
  function
  | Unit -> "Unit"
  | Exp(e) -> "Exp(" ^ (string_of_typed_expr e) ^ ")"
  | If(e1, e2, e3) -> "If(" ^ (string_of_typed_expr e1) ^ " then " ^ (string_of_typed_term e2) ^ " else " ^ (string_of_typed_term e3) ^ ")"
  | Match(x, pes) -> "Match(" ^ x ^ ", [" ^ (String.concat "; " (List.map (fun (p, e) -> (string_of_pattern p) ^ " -> " ^ (string_of_typed_term e)) pes)) ^ "])"
  | Let((x, t), e1, e2) -> "Let(" ^ x ^ " : " ^ (Type.to_string t) ^ " = " ^ (string_of_typed_term e1) ^ " in " ^ (string_of_typed_term e2) ^ ")"
  | MakeCls((x, t), { entry = Id.L(l); actual_fv = ys }, e) -> "MakeCls(" ^ x ^ " : " ^ (Type.to_string t) ^ " = " ^ l ^ ", [" ^ (String.concat ", " ys) ^ "] in " ^ (string_of_typed_term e) ^ ")"

let string_of_typed_id (x, t) = x ^ " : " ^ (Type.to_string t)
      
let string_of_fundef { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e } =
  "{ name = " ^ x ^ ", args = [" ^ (String.concat ", " (List.map string_of_typed_id yts)) ^ "], formal_fv = [" ^ (String.concat ", " (List.map string_of_typed_id zts)) ^ "], body = " ^ (string_of_typed_term e) ^ "}"
    
let string_of_def = 
  function
  | TypeDef(x, t) -> "TypeDef(" ^ x ^ ", " ^ (Type.Tycon.to_string t) ^ ")"
  | VarDef((x, t), e) -> "VarDef((" ^ x ^ ", " ^ (Type.to_string t) ^ "), " ^ (string_of_typed_term e) ^ ")"
  | FunDef(fundef) -> "FunDef(" ^ (string_of_fundef fundef) ^ ")"

let rec vars_of_pattern = 
  function
  | PtUnit | PtBool _ | PtInt _ | PtFloat _ | PtAtom _ | PtString _ -> Id.Set.empty
  | PtVar(x, _) -> Id.Set.singleton x
  | PtAlias (p, x, _) -> Id.Set.add x & Id.Set.union Id.Set.empty & vars_of_pattern p
  | PtTuple(ps) | PtList ps | PtConstr(_, ps) ->
    List.fold_left (fun s p -> Id.Set.union s (vars_of_pattern p)) Id.Set.empty ps
  | PtCons (p1, p2) -> 
    List.fold_left (fun s p -> Id.Set.union s (vars_of_pattern p)) Id.Set.empty [p1; p2]
  | PtRecord(xps) -> List.fold_left (fun s (_, p) -> Id.Set.union s (vars_of_pattern p)) Id.Set.empty xps
      
let rec fv_of_expr (e, _) = 
  match e with
  | Bool(_) | Int(_) | Float _ | Char _ | String _ | Atom _ | Bitstring _ -> Id.Set.empty
  | Record(xes) -> List.fold_left (fun s (_, e) -> Id.Set.union s (fv_of_expr e)) Id.Set.empty xes
  | Field(e, _) -> fv_of_expr e
  | Tuple(es) | List(es) | Array(es) ->
    List.fold_left (fun s e -> Id.Set.union s (fv_of_expr e)) Id.Set.empty es
  | Not(e) | Neg(e) -> fv_of_expr e
  | And(e1, e2) | Or(e1, e2) 
  | Add(e1, e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) | Concat(e1, e2)
  | Eq(e1, e2) | LE(e1, e2) | Get(e1, e2) ->
    Id.Set.union (fv_of_expr e1) (fv_of_expr e2)
  | Var(`Local x) -> Id.Set.singleton x
  | Var (`Module _) -> Id.Set.empty
  | Constr(_, es) -> List.fold_left (fun s e -> Id.Set.union s (fv_of_expr e)) Id.Set.empty es
  | AppCls(e, es) -> List.fold_left (fun s e -> Id.Set.union s (fv_of_expr e)) Id.Set.empty (e :: es)
  | AppDir(_, es) -> List.fold_left (fun s e -> Id.Set.union s (fv_of_expr e)) Id.Set.empty es
  | Put(e1, e2, e3) ->
    Id.Set.union (fv_of_expr e3) & Id.Set.union (fv_of_expr e1) (fv_of_expr e2)
      
let rec fv (e, _) = 
  match e with
  | Unit -> Id.Set.empty
  | Exp(e) -> fv_of_expr e
  | If(e, e1, e2) -> Id.Set.union (fv_of_expr e) (Id.Set.union (fv e1) (fv e2))
  | Match(x, pes) -> (List.fold_left (fun s (p, e) -> Id.Set.diff (Id.Set.union s (fv e)) (vars_of_pattern p)) Id.Set.empty pes)
  | Let((x, t), e1, e2) -> Id.Set.union (fv e1) (Id.Set.remove x (fv e2))
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> Id.Set.remove x (Id.Set.union (Id.Set.of_list ys) (fv e))
      
let toplevel : def list ref = ref []

let ids_of_defs defs = 
  List.fold_left 
    (fun ids def -> 
      match def with
      | VarDef((x, _), _) 
      | FunDef{ name = (Id.L(x), _); formal_fv = [] } -> x :: ids (* 自由変数がないものは直接呼び出せるためグローバルなIDとして返す *)
      | TypeDef _ | FunDef _ -> ids (* 自由変数があるものはクロージャ呼び出し経由での適応となる *)
    ) [] defs
    
let rec pattern env = 
  function
  | KNormal_t.PtUnit -> env, PtUnit
  | KNormal_t.PtBool(b) -> env, PtBool(b)
  | KNormal_t.PtInt(n) -> env, PtInt(n)
  | KNormal_t.PtFloat v -> env, PtFloat v
  | KNormal_t.PtAtom v -> env, PtAtom v
  | KNormal_t.PtString v -> env, PtString v
  | KNormal_t.PtVar(x, t) -> Id.Map.add x t env, PtVar(x, t)
  | KNormal_t.PtAlias (p, x, t) ->
    let env', p' = pattern env p in
    Id.Map.add x t env', PtAlias (p', x, t)
  | KNormal_t.PtTuple(ps) -> 
      let env, ps' = List.fold_left 
        (fun (env, ps) p -> 
          let env', p' = pattern env p in 
          env', p' :: ps) 
        (env, []) ps in
      env, PtTuple(List.rev ps')
  | KNormal_t.PtList(ps) -> 
      let env, ps' = List.fold_left 
        (fun (env, ps) p -> 
          let env', p' = pattern env p in 
          env', p' :: ps) 
        (env, []) ps in
      env, PtList(List.rev ps')
  | KNormal_t.PtCons (p1, p2) -> 
    let env', p1' = pattern env p1 in
    let env'', p2' = pattern env' p2 in
    env'', PtCons (p1', p2')
  | KNormal_t.PtField(xps) -> 
      let env, xps' = List.fold_left 
        (fun (env, xps) (x, p) -> 
          let env', p' = pattern env p in 
          env', (x, p') :: xps) 
        (env, []) xps in
      env, PtRecord(List.rev xps')
  | KNormal_t.PtConstr(x, ps) -> 
      let env, ps' = List.fold_left 
        (fun (env, ps) p -> 
          let env', p' = pattern env p in 
          env', p' :: ps) 
        (env, []) ps in
      env, PtConstr(x, List.rev ps')

let rec h env known (expr, ty) = 
  Log.debug "Closure.h %s\n" (KNormal.string_of_expr expr);
  let e' =
    match expr with
    | KNormal_t.Bool(b) -> Bool(b)
    | KNormal_t.Int(i) -> Int(i)
    | KNormal_t.Float v -> Float v
    | KNormal_t.Char s -> Char s
    | KNormal_t.String s -> String s
    | KNormal_t.Atom s -> Atom s
    | KNormal_t.Bitstring s -> Bitstring s
    | KNormal_t.Record(xes) -> Record(List.map (fun (x, e) -> x, h env known e) xes)
    | KNormal_t.Field(e, x) -> Field(h env known e, x)
    | KNormal_t.Tuple(es) -> Tuple(List.map (h env known) es)
    | KNormal_t.List(es) -> List(List.map (h env known) es)
    | KNormal_t.Array(es) -> Array(List.map (h env known) es)
    | KNormal_t.Not(e) -> Not(h env known e)
    | KNormal_t.Neg(e) -> Neg(h env known e)
    | KNormal_t.And(e1, e2) -> And(h env known e1, h env known e2)
    | KNormal_t.Or(e1, e2)  -> Or(h env known e1, h env known e2)
    | KNormal_t.Add(e1, e2) -> Add(h env known e1, h env known e2)
    | KNormal_t.Sub(e1, e2) -> Sub(h env known e1, h env known e2)
    | KNormal_t.Mul(e1, e2) -> Mul(h env known e1, h env known e2)
    | KNormal_t.Div(e1, e2) -> Div(h env known e1, h env known e2)
    | KNormal_t.Concat(e1, e2) -> Concat(h env known e1, h env known e2)
    | KNormal_t.Eq(e1, e2)  -> Eq(h env known e1, h env known e2)
    | KNormal_t.LE(e1, e2)  -> LE(h env known e1, h env known e2)
    | KNormal_t.Var(x) -> Var(x)
    | KNormal_t.Constr(x, es) -> Constr(x, List.map (h env known) es)
    | KNormal_t.App((KNormal_t.Var(`Local x), ft), ys) when Id.Set.mem x known ->
      Log.debug "directly applying %s\n" x;
      AppDir(Binding.of_string x, List.map (h env known) ys)
    | KNormal_t.App((KNormal_t.Var(`Module x), ft), ys) ->
      Log.debug "directly applying %s\n" (Binding.to_string x);
      AppDir(x, List.map (h env known) ys)
    | KNormal_t.App(e, es) -> 
        AppCls(h env known e, List.map (h env known) es)
    | KNormal_t.ExtFunApp(x, ys) -> 
      AppDir(Binding.of_string x, List.map (h env known) ys)
    | KNormal_t.Get (e1, e2)  -> Get (h env known e1, h env known e2)
    | KNormal_t.Put (e1, e2, e3) ->
      Put (h env known e1, h env known e2, h env known e3)
  in
  (e', ty)
  
let rec g venv known (expr, ty) = (* クロージャ変換ルーチン本体 (caml2html: closure_g) *)
  Log.debug "Closure.g %s\n" (KNormal.string_of_term expr);
  let expr' = 
    match expr with 
    | KNormal_t.Unit -> Unit
    | KNormal_t.Exp(e) -> Exp(h venv known e)
    | KNormal_t.If(e, e1, e2) -> If(h venv known e, g venv known e1, g venv known e2)
    | KNormal_t.Match(x, pes) -> Match(x, (List.map (fun (p, e) -> let env', p' = pattern venv p in p', (g env' known e)) pes))
    | KNormal_t.Let((x, t), e1, e2) -> Let((x, t), g venv known e1, g (Id.Map.add x t venv) known e2)
    | KNormal_t.LetRec({ KNormal_t.name = (x, ty_f); KNormal_t.args = yts; KNormal_t.body = e1 }, e2) -> (* 関数定義の場合 (caml2html: closure_letrec) *)
        (* 関数定義let rec x y1 ... yn = e1 in e2の場合は、
	       xに自由変数がない(closureを介さずdirectに呼び出せる)
	       と仮定し、knownに追加してe1をクロージャ変換してみる *)
        let toplevel_backup = !toplevel in
        let venv' = Id.Map.add x ty_f venv in
        let known' = Id.Set.add x known in
        let e1' = g (Id.Map.add_alist yts venv') known' e1 in
        (* 本当に自由変数がなかったか、変換結果e1'を確認する *)
        (* 注意: e1'にx自身が変数として出現する場合はclosureが必要!
           (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml参照) *)
        let zs = Id.Set.diff (fv e1') (Id.Set.of_list ((List.map fst yts) @ (ids_of_defs !toplevel))) in
        let known', e1' =
	      if Id.Set.is_empty zs then (Log.debug "function %s doesn't have free variables.\n" x; known', e1')
          (* 駄目だったら状態(toplevelの値)を戻して、クロージャ変換をやり直す *)
          else (Log.debug "free variable(s) %s found in function %s@.\n" (Id.pp_list (Id.Set.elements zs)) x;
	            Log.debug "function %s cannot be directly applied in fact@.\n" x;
	            toplevel := toplevel_backup;
	            let e1' = g (Id.Map.add_alist yts venv') known e1 in
	            known, e1') in
        let zs = Id.Set.elements (Id.Set.diff (fv e1') (Id.Set.add x (Id.Set.of_list ((List.map fst yts) @ (ids_of_defs !toplevel))))) in
        let zts = List.map (fun z -> z, Id.Map.find z venv') zs in (* ここで自由変数zの型を引くために引数venvが必要 *)
        toplevel := FunDef{ name = (Id.L(x), ty_f); args = yts; formal_fv = zts; body = e1' } :: !toplevel; (* トップレベル関数を追加 *)
        let e2' = g venv' known' e2 in
        if Id.Set.mem x (fv e2') then (* xが変数としてe2'に出現するか。ただし、自由変数がないときはクロージャをつくらず関数ポイントとして使用する *)
	    MakeCls((x, ty_f), { entry = Id.L(x); actual_fv = zs }, e2') (* 出現していたら削除しない *)
        else (Log.debug "eliminating closure(s) %s@.\n" x;
	          fst e2') (* 出現しなければMakeClsを削除 *) in
  (expr', ty)
    
let f' { Env.venv = venv } e = 
  let known = Id.Map.fold (fun x _ known -> Id.Set.add x known) venv Id.Set.empty in
  g venv known e

let f defs =
  toplevel := [];
  ignore (KNormal.fold
            (fun ({ Env.venv = venv; tenv = tenv } as env, defs) def ->
              let env', def' = 
                match def with 
                | KNormal_t.TypeDef(x, t) -> 
                    { env with 
                      Env.venv = Id.Map.add_alist (Type.Tycon.vars t) venv; 
                      Env.tenv = Id.Map.add_alist (Type.Tycon.types t) tenv }, 
                  TypeDef(x, t)
                | KNormal_t.VarDef((x, t), e) -> 
                    Env.add_var env x t, (VarDef((x, t), f' env e))
                | KNormal_t.RecDef({ KNormal_t.name = (x, ty_f); args = yts; body = e1 }) ->
                    let env' = Env.add_var env x ty_f in
                    env', (FunDef({ name = (Id.L(x), ty_f); args = yts; formal_fv = []; body = f' env' e1 })) in
              toplevel := def' :: !toplevel;
              (env', def' :: defs))
            !Env.empty defs);
  Prog(List.rev !toplevel)
    
