(* type inference/reconstruction *)

open Syntax
open Locating
open Spotlib.Base

exception Unify of Type.t * Type.t
exception Error of expr Locating.t * Type.t * Type.t
    
let rec subst ({ Env.tycons = tycons } as env) tyvars reached t =
  Log.debug "# Typing.subst %s\n" (Type.string_of_t t);
  let rec subst' reached ty = 
    Log.debug "#    Typing.subst' %s\n" (Type.string_of_t ty);
    match ty with
    | Type.Var(x) when M.mem x tyvars -> M.find x tyvars
    | Type.Var(x) -> Type.Var(x)
    | Type.Field(tr, t) -> Type.Field(subst' reached tr, subst' reached t)
    | Type.App(Type.Record(x, fs), ys) -> Type.App(Type.Record(x, fs), List.map (subst' (S.add x reached)) ys)
    | Type.App(Type.Variant(x, constrs), ys) -> 
        let constrs' = List.map (fun (c, ts) -> c, List.map (fun t -> subst' (S.add x reached) t) ts) constrs in
        Type.App(Type.Variant(x, constrs'), List.map (subst' (S.add x reached)) ys)
    | Type.App(Type.TyFun(xs, t), ys) -> subst' reached (subst env (M.add_list2 xs ys M.empty) reached t)
    | Type.App(Type.NameTycon(x, _) as t, ys) when S.mem x reached -> Type.App(t, List.map (subst' reached) ys)
    | Type.App(Type.NameTycon(x, { contents = Some(tycon) }), ys) -> subst' reached (Type.App(tycon, ys))
    | Type.App(Type.NameTycon(x, _), ys) -> subst' reached (Type.App(M.find x tycons, ys))
    | Type.App(x, ys) -> Type.App(x, (List.map (subst' reached) ys))
    | Type.Poly([], t) -> subst' reached t
    | Type.Poly(xs, t) -> assert false; (* impossible *)
    | Type.Meta{ contents = Some(t) } -> 
        subst' reached t
    | Type.Meta{ contents = None } as t -> t in
  subst' reached t

let subst env tyvars = subst env tyvars S.empty
    
let rec occur x = (* occur check (caml2html: typing_occur) *)
  function 
  | Type.Var _ -> false
  | Type.Field(_, t) -> occur x t
  | Type.App(Type.TyFun(_, u), ts) -> occur x u || List.exists (occur x) ts
  | Type.App(Type.Variant(_, constrs), ts) -> List.exists (fun (_, ts) -> List.exists (occur x) ts) constrs || List.exists (occur x) ts
  | Type.App(_, ts) -> List.exists (occur x) ts
  | Type.Poly(_, t) -> occur x t
  | Type.Meta{ contents = Some(t) } -> occur x t
  | Type.Meta(y) -> x == y
      
let unify ({ Env.tycons = tycons } as env) ty1 ty2 = (* 型が合うように、メタ型変数への代入をする. 成功したら () を返す. (caml2html: typing_unify) *)
  Log.debug "# Typing.unify %s %s\n" (Type.string_of_t ty1) (Type.string_of_t ty2);
  let rec unify' t1 t2 =  
    Log.debug "#     Typing.unify' %s %s\n" (Type.string_of_t t1) (Type.string_of_t t2);
    match t1, t2 with
    | Type.App(Type.Unit, xs), Type.App(Type.Unit, ys) 
    | Type.App(Type.Bool, xs), Type.App(Type.Bool, ys) 
    | Type.App(Type.Int, xs), Type.App(Type.Int, ys) 
    | Type.App(Type.String, xs), Type.App(Type.String, ys) 
    | Type.App(Type.Tuple, xs), Type.App(Type.Tuple, ys) 
    | Type.App(Type.Arrow, xs), Type.App(Type.Arrow, ys) -> List.iter2 unify' xs ys
    | Type.App(Type.Record(x, fs), xs), Type.App(Type.Record(y, fs'), ys) when fs = fs' -> List.iter2 unify' xs ys
    | Type.App(Type.Variant(x, constrs), xs), Type.App(Type.Variant(y, constrs'), ys) when x = y -> 
        List.iter2 (fun (_, ts) (_, ts') -> List.iter2 unify' ts ts') constrs constrs';
        List.iter2 unify' xs ys;
    | Type.App(Type.TyFun(xs, u), ys), t2 -> unify' (subst env (M.add_list2 xs ys M.empty) u) t2
    | t1, Type.App(Type.TyFun(xs, u), ys) -> unify' t1 (subst env (M.add_list2 xs ys M.empty) u)
    | Type.App(Type.NameTycon(x, _), xs), Type.App(Type.NameTycon(y, _), ys) when x = y -> List.iter2 unify' xs ys
    | Type.App(Type.NameTycon(x, { contents = None }), ys), t2 ->
      unify' (Type.App(M.find x tycons, ys)) t2
    | t1, Type.App(Type.NameTycon(x, { contents = None }), ys) ->
      unify' t1 (Type.App(M.find x tycons, ys))
    | Type.App(Type.NameTycon(x, { contents = Some(t1) }), xs), t2 ->
      unify' (Type.App(t1, xs)) t2
    | t1, Type.App(Type.NameTycon(x, { contents = Some(t2) }), ys) ->
      unify' t1 (Type.App(t2, ys))
    | Type.Poly([], u1), t2 -> unify' u1 t2
    | t1, Type.Poly([], u2) -> unify' t1 u2
    | Type.Poly(xs, u1), Type.Poly(ys, u2) -> unify' u1 (subst env (M.add_list2 ys (List.map (fun x -> Type.Var(x)) xs) M.empty) u2)
    | Type.Var(x), Type.Var(y) when x = y -> ()
    | Type.Field(rt1, t1), Type.Field(rt2, t2) -> unify' rt1 rt2; unify' t1 t2
    | Type.Meta{ contents = Some(t1') }, t2 -> unify' t1' t2
    | t1, Type.Meta{ contents = Some(t2') } -> unify' t1 t2'
    | Type.Meta(x), Type.Meta(y) when x == y -> ()
    | Type.Meta(x), t2 ->
        if occur x t2 then raise (Unify(t1, t2))
        else x := Some(t2)
    | t1, Type.Meta(y) -> unify' t2 t1
    | _, _ -> 
        Log.debug "unify failed.\n  t1 = %s\n  t2 = %s\n" (Type.string_of_t t1) (Type.string_of_t t2);
        raise (Unify(t1, t2)) in
  unify' ty1 ty2
    
let test_unify =
  assert ((unify !Env.empty (Type.App(Type.Int, [])) (Type.App(Type.Int, []))) = ())
    
(*        
let rec expand tenv = 
  function
  | Type.App(Type.TyFun(xs, u), ys) -> expand tenv (subst ((M.add_list2 xs ys M.empty), tenv) u)
  | Type.Meta{ contents = Some(t) } -> expand tenv t
  | Type.NameTy(x, _) -> expand tenv (M.find x tenv)
  | t -> t
*)      
let generalize { Env.venv = venv; tycons = tycons } ty = 
  Log.debug "# Typing.generalize %s\n" (Type.string_of_t ty);
  let rec exists v = 
    function
    | Type.App(Type.NameTycon(_, { contents = Some(tycon) }), ts) -> exists v (Type.App(tycon, ts))
    | Type.App(Type.NameTycon(x, { contents = None }), ts) -> exists v (Type.App(M.find x tycons, ts))
    | Type.App(Type.TyFun(_, u), ts) -> exists v u || List.exists (exists v) ts
    | Type.App(_, ts) -> List.exists (exists v) ts
    | Type.Poly(_, t) -> exists v t
    | Type.Meta{ contents = Some(t') } -> exists v t'
    | Type.Meta(x) when v == x -> true
    | _ -> false in
  let rec metavars vs = 
    function
    | Type.Var _ -> vs
    | Type.Field(_, t) -> metavars vs t
    | Type.App(Type.TyFun(_, u), ts) -> List.fold_left metavars (metavars vs u) ts
    | Type.App(Type.Variant(_, constrs), ts) -> 
        let vs = List.fold_left (fun vs (_, ts) -> List.fold_left metavars vs ts) vs constrs in
        List.fold_left metavars vs ts
    | Type.App(_, ts) -> List.fold_left metavars vs ts
    | Type.Poly(_, t) -> metavars vs t
    | Type.Meta{ contents = Some(t') } -> metavars vs t'
    | Type.Meta(x) when M.exists (fun _ t' -> exists x t') venv -> vs
    | Type.Meta(x) -> if (List.memq x vs) then vs else x :: vs in
  let ms = metavars [] ty in
  let tyvars = List.map 
    (fun m -> 
      match !m with 
      | None -> let var = Type.newtyvar () in 
                m := Some(Type.Var(var)); 
                var 
      | _ -> assert false) 
    ms in
  let t = Type.Poly(tyvars, ty) in
  let _ = Log.debug "  => %s\n" (Type.string_of_t t) in
  t
    
let instantiate env ty =
  Log.debug "Typing.instantiate %s\n" (Type.string_of_t ty);
  let t = 
    match ty with
    | Type.Poly(xs, t) -> 
        subst env (M.add_list (List.map (fun x -> (x, Type.Meta(Type.newmetavar ()))) xs) M.empty) t
    | t -> t in
  let _ = Log.debug "  => %s\n" (Type.string_of_t t) in
  t
      
(* for pretty printing (and type normalization) *)
let rec deref_tycon ({ Env.tycons = tycons } as env) reached tycon =
  match tycon with
  | Type.Int | Type.Bool | Type.String | Type.Unit | Type.Arrow | Type.Tuple | Type.Module _ as tycon -> tycon, reached
  | Type.Record(x, _) as tycon -> tycon, M.add x tycon reached
  | Type.Variant(x, _) when M.mem x reached -> 
      tycon, reached
  | Type.Variant(x, constrs) ->
      let constrs', reached = 
        List.fold_left (fun (constrs, reached) (c, ys) -> 
          let ys', reached = 
            List.fold_left 
              (fun (ys', reached) y -> 
                let y', reached = deref_type env reached y in 
                y'::ys', reached) ([], reached) (List.rev ys) in
          (c, ys') :: constrs, reached) ([], reached) (List.rev constrs) in
      let tycon' = Type.Variant(x, constrs') in
      tycon', reached
  | Type.NameTycon(x, { contents = Some(tycon) }) ->       
      tycon, M.add x tycon reached
  | Type.NameTycon("int", { contents = None }) ->
    M.find "int" tycons, reached
  | Type.NameTycon("bool", { contents = None }) ->
    M.find "bool" tycons, reached
  | Type.NameTycon("string", { contents = None }) ->
    M.find "string" tycons, reached
  | Type.NameTycon("unit", { contents = None }) ->
    M.find "unit" tycons, reached
  | Type.NameTycon(x, r) when M.mem x reached -> 
      let tycon = M.find x reached in 
      r := Some(tycon);
      tycon, reached
  | Type.NameTycon(x, { contents = None }) -> assert false
  | Type.TyFun(xs, Type.App(Type.Variant(x, constrs), ys)) -> 
      let reached = M.add x tycon reached in
      let constrs', reached = 
        List.fold_left (fun (constrs, reached) (c, ys) -> 
          let ys', reached = 
            List.fold_left 
              (fun (ys', reached) y -> 
                let y', reached = deref_type env reached y in 
                y'::ys', reached) ([], reached) (List.rev ys) in
          (c, ys') :: constrs, reached) ([], reached) (List.rev constrs) in
      let ys', reached = List.fold_left (fun (ys', reached) y -> let y', reached = deref_type env reached y in y'::ys', reached) ([], reached) (List.rev ys) in
      let tycon' = Type.TyFun(xs, Type.App(Type.Variant(x, constrs'), ys')) in
      tycon', reached
  | Type.TyFun(xs, t) -> 
      let t', reached' = deref_type env reached t in
        Type.TyFun(xs, t'), reached'
  
and deref_type env reached t = (* 型変数を中身でおきかえる関数 (caml2html: typing_deref) *)
  match t with
  | Type.Var(x) -> Type.Var(x), reached
  | Type.Field(x, t) -> 
      let t', reached' = deref_type env reached t in 
      Type.Field(x, t'), reached'
  | Type.App(x, ys) -> 
      let x', reached = deref_tycon env reached x in
      let ys', reached = List.fold_left (fun (ys', reached) y -> let y', reached = deref_type env reached y in y'::ys', reached) ([], reached) (List.rev ys) in
      subst env M.empty (Type.App(x', ys')), reached
  | Type.Poly(xs, t) -> 
      let t', reached' = deref_type env reached t in
      Type.Poly(xs, t'), reached'
  | Type.Meta({ contents = None }) -> 
      Type.Var(Type.newtyvar ()), reached
  | Type.Meta({ contents = Some(t) } as r) ->
      let t', reached' = deref_type env reached t in
      r := Some(t');
      t', reached'

let deref_tycon env tycon = 
  let tycon', _ = deref_tycon env M.empty tycon in
  tycon'

let deref_type env ty = 
  let t', _ = deref_type env M.empty ty in
  t'

let rec deref_pattern env lp =
  let (d, env) = match desc lp with
  | PtUnit | PtBool _ | PtInt _ as p -> p, env
  | PtVar(x, t) -> PtVar(x, deref_type env t), Env.add_var env x t
  | PtTuple(ps) -> 
    let ps', env' = List.fold_right
        (fun p (ps, env) ->
           let p', env' = deref_pattern env p in
           p' :: ps, env')
        ps ([], env) in
    PtTuple(ps'), env'
  | PtRecord(xps) -> 
      let xps', env' = List.fold_right (fun (x, p) (xps, env) -> let p', env' = deref_pattern env p in (x, p') :: xps, env') xps ([], env) in
      PtRecord(xps'), env'
  | PtConstr(x, ps) -> 
      let ps', env' = List.fold_right (fun p (ps, env) -> let p', env' = deref_pattern env p in p' :: ps, env') ps ([], env) in
      PtConstr(x, ps'), env'
  in
  set lp d, env

let deref_id_type env (x, ty) = (x, deref_type env ty)

let rec deref_typed_expr ({ Env.venv = venv } as env) le =
  let (e, t) = desc le in
  set le (deref_expr env e, deref_type env t)

and deref_expr ({ Env.venv = venv } as env) = function
  | Int _ | Bool _ | String _ | Unit | Var _ | Module _ as e -> e
  | Record(xes) -> Record(List.map (fun (x, e) -> x, deref_typed_expr env e) xes)
  | Field(e, x) -> Field(deref_typed_expr env e, x)
  | Tuple(es) -> Tuple(List.map (deref_typed_expr env) es)
  | Not(e) -> Not(deref_typed_expr env e)
  | And(e1, e2) -> And(deref_typed_expr env e1, deref_typed_expr env e2)
  | Or(e1, e2) -> Or(deref_typed_expr env e1, deref_typed_expr env e2)
  | Neg(e) -> Neg(deref_typed_expr env e)
  | Add(e1, e2) -> Add(deref_typed_expr env e1, deref_typed_expr env e2)
  | Sub(e1, e2) -> Sub(deref_typed_expr env e1, deref_typed_expr env e2)
  | Eq(e1, e2) -> Eq(deref_typed_expr env e1, deref_typed_expr env e2)
  | LE(e1, e2) -> LE(deref_typed_expr env e1, deref_typed_expr env e2)
  | Mul(e1, e2) -> Mul(deref_typed_expr env e1, deref_typed_expr env e2)
  | Div(e1, e2) -> Div(deref_typed_expr env e1, deref_typed_expr env e2)
  | Concat(e1, e2) -> Concat(deref_typed_expr env e1, deref_typed_expr env e2)
  | If(e1, e2, e3) -> If(deref_typed_expr env e1, deref_typed_expr env e2, deref_typed_expr env e3)
  | Match(e, pes) ->  Match(deref_typed_expr env e, (List.map (fun (p, e) -> let p', env' = deref_pattern env p in p', deref_typed_expr env' e) pes))
  | LetVar((x, t), e1, e2) -> 
      LetVar(deref_id_type env (x, t), deref_typed_expr env e1, deref_typed_expr (Env.add_var env x t) e2)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      LetRec({ name = deref_id_type env (x, t);
               args = List.map (deref_id_type env) yts;
               body = deref_typed_expr { env with Env.venv = M.add_list yts (M.add x t venv) } e1 },
             deref_typed_expr (Env.add_var env x t) e2)
  | App(e, es) -> App(deref_typed_expr env e, List.map (deref_typed_expr env) es)
  | Constr(x, es) -> Constr(x, List.map (deref_typed_expr env) es)

let deref_def env def =
  set def (match def.desc with
      | TypeDef(x, t) -> 
        TypeDef(x, deref_tycon env t)
      | VarDef((x, t), et) -> 
        VarDef((x, deref_type env t), deref_typed_expr env et)
      | RecDef({ name = (x, ty_f); args = yts; body = et }) -> 
        RecDef({ name = (x, deref_type env ty_f); 
                 args = List.map (fun (y, t) -> y, deref_type env t) yts; 
                 body = deref_typed_expr env et })
      | _ -> assert false)

let rec pattern ({ Env.venv = venv; tenv = tenv } as env) p =
  Log.debug "Typing.pattern (%s)\n" (string_of_pattern p);
  match desc p with
  | PtUnit -> env, Type.App(Type.Unit, [])
  | PtBool(b) -> env, Type.App(Type.Bool, [])
  | PtInt(n) -> env, Type.App(Type.Int, [])
  | PtVar(x, t') -> Env.add_var env x t', t'
  | PtTuple(ps) -> 
      let env', ts' = List.fold_left (fun (env, ts) p -> let env', t' = pattern env p in env', t' :: ts) (env, []) (List.rev ps) in
      env', Type.App(Type.Tuple, ts')
  | PtRecord(xps) -> 
      let env', ts' = List.fold_left (fun (env, ts) (_, p) -> let env', t' = pattern env p in env', t' :: ts) (env, []) (List.rev xps) in
      begin
        match M.find (fst (List.hd xps)) tenv with
        | Type.Poly(xs, Type.Field(t, _)) ->
            let t' = instantiate env (Type.Poly(xs, t)) in
            begin
              match t' with
              | Type.App(Type.Record(_), ts) ->
                  List.iter2 (unify env) ts ts';
                  env', t'
              | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t); assert false
            end
        | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t); assert false
      end
  | PtConstr(x, ps) -> 
      let env', pts' = List.fold_left (fun (env, pts) p -> let env', t' = pattern env p in env', (p, t') :: pts) (env, []) (List.rev ps) in
      begin
        match instantiate env (M.find x venv) with
        | Type.App(Type.Variant(_, constrs), _) as t -> 
            assert (ps = []);
            assert (List.exists (function (y, []) -> x = y | _ -> false) constrs);
            env, t
        | Type.App(Type.Arrow, ys) -> 
            begin 
              match L.last ys with
              | Type.App(Type.Variant(_, constrs), _) as t -> 
                  List.iter
                    (function
                    | y, ts' when x = y -> List.iter2 (fun (_, t) t' -> unify env' t t') pts' ts'
                    | _  -> ()) constrs;
                  env', t
              | t -> Printf.eprintf "invalid type : %s\n" (Type.string_of_t t); assert false
            end
        | t -> Printf.eprintf "invalid type : %s\n" (Type.string_of_t t); assert false
      end
        
let rec g ({ Env.venv = venv; tenv = tenv } as env) e = (* 型推論ルーチン (caml2html: typing_g) *)
  let expr, ty = desc e in
  Log.debug "# %s: Typing.g %s\n" (Location.to_string e.loc) (string_of_expr expr);
  try
    let expr', ty' =
      match expr with
      | Unit -> expr, Type.App(Type.Unit, [])
      | Bool(_) -> expr, Type.App(Type.Bool, [])
      | Int(_) -> expr, Type.App(Type.Int, [])
      | String _ -> expr, Type.App(Type.String, [])
      | Record(xets) -> 
        let xets', ts' = List.fold_left 
            (fun (xets, ts) (x, e) ->
               let e', t' = g env e in
               (x, { e with desc = (e', t') }) :: xets, t' :: ts)
            ([], []) (List.rev xets)
        in 
        begin match M.find (fst (List.hd xets)) tenv with
          | Type.Poly(xs, Type.Field(t, _)) -> 
            let t' = instantiate env (Type.Poly(xs, t)) in
            begin
              match t' with
              | Type.App(Type.Record(_, _), ts) ->
                List.iter2 (unify env) ts ts';
                Record(xets'), t'
              | t ->
                Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t);
                assert false
            end
          | t ->
            Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t);
            assert false
        end
      | Field ({ desc = (Module mx, _) }, x) ->
        begin match Module.find_opt mx with
        | Some _ -> ()
        | None ->
          if not & Sig.load_module mx then
            raise (Unbound_module_error (e.loc, mx))
        end;
        let m = Module.find mx in
        Log.debug "#   => module val %s.%s\n" mx x;
        begin match Module.find_val_opt m x with
        | None -> raise (Unbound_value_error (e.loc, Module.(m.name) ^ "." ^ x))
        | Some t -> expr, t
        end
      | Field(et, x) ->
          let _, ty_rec' as et' = g env et in
          let ty_f = instantiate env (M.find x tenv) in
          let ty_f' = Type.Meta(Type.newmetavar ()) in
          unify env ty_f (Type.Field(ty_rec', ty_f'));
          Field({ e with desc = et' }, x), ty_f'
      | Tuple(ets) ->
        let ets', ts' =
          List.fold_left
            (fun (ets, ts) e ->
               let e', t' = g env e in
               (set e (e', t')) :: ets, t' :: ts)
            ([], []) (List.rev ets)
        in
        Tuple(ets'), Type.App(Type.Tuple, ts')
      | Not(et) ->
        let e', t' = g env et in
        unify env (Type.App(Type.Bool, [])) t';
        Not (set et (e', t')), Type.App(Type.Bool, [])
      | Concat(et1, et2) ->
        let e1', t1' = g env et1 in
        let e2', t2' = g env et2 in
        unify env (Type.App(Type.String, [])) t1';
        unify env (Type.App(Type.String, [])) t2';
        Concat (set et1 (e1', t1'), set et2 (e2', t2')), Type.App(Type.String, [])
      | And(et1, et2) ->
        let e1', t1' = g env et1 in
        let e2', t2' = g env et2 in
        unify env (Type.App(Type.Bool, [])) t1';
        unify env (Type.App(Type.Bool, [])) t2';
        And (set et1 (e1', t1'), set et2 (e2', t2')), Type.App(Type.Bool, [])
      | Or(et1, et2) ->
        let e1', t1' = g env et1 in
        let e2', t2' = g env et2 in
        unify env (Type.App(Type.Bool, [])) t1';
        unify env (Type.App(Type.Bool, [])) t2';
        Or (set et1 (e1', t1'), set et2 (e2', t2')), Type.App(Type.Bool, [])
      | Neg(e) ->
        let e', t' = g env e in
        unify env (Type.App(Type.Int, [])) t';
        Neg (set e (e', t')), Type.App(Type.Int, [])
      | Add(et1, et2) ->
        let e1', t1' = g env et1 in
        let e2', t2' = g env et2 in
        unify env (Type.App(Type.Int, [])) t1';
        unify env (Type.App(Type.Int, [])) t2';
        Add (set et1 (e1', t1'), set et2 (e2', t2')), Type.App(Type.Int, [])
      | Sub(et1, et2) ->
        let e1', t1' = g env et1 in
        let e2', t2' = g env et2 in
        unify env (Type.App(Type.Int, [])) t1';
        unify env (Type.App(Type.Int, [])) t2';
        Sub(set et1 (e1', t1'), set et2 (e2', t2')), Type.App(Type.Int, [])
      | Mul(et1, et2) -> 
        let e1', t1' = g env et1 in
        let e2', t2' = g env et2 in
        unify env (Type.App(Type.Int, [])) t1';
        unify env (Type.App(Type.Int, [])) t2';
        Mul(set et1 (e1', t1'), set et2 (e2', t2')), Type.App(Type.Int, [])
      | Div(et1, et2) ->
        let e1', t1' = g env et1 in
        let e2', t2' = g env et2 in
        unify env (Type.App(Type.Int, [])) t1';
        unify env (Type.App(Type.Int, [])) t2';
        Div(set et1 (e1', t1'), set et2 (e2', t2')), Type.App(Type.Int, [])
      | Eq(et1, et2) ->
        let e1', t1' = g env et1 in
        let e2', t2' = g env et2 in
        unify env t1' t2';
        Eq(set et1 (e1', t1'), set et2 (e2', t2')), Type.App(Type.Bool, [])
      | LE(et1, et2) ->
        let e1', t1' = g env et1 in
        let e2', t2' = g env et2 in
        unify env t1' t2';
        (* OCamlはLEは多相だけど、一旦Intにしておく。多相にすると、生成されるC言語ではポインタ同士の演算になるから *)
        unify env (Type.App(Type.Int, [])) t1';
        unify env (Type.App(Type.Int, [])) t2';
        LE(set et1 (e1', t1'), set et2 (e2', t2')), Type.App(Type.Bool, [])
      | If(et1, et2, e3) ->
          let e1', t1' = g env et1 in
          unify env t1' (Type.App(Type.Bool, []));
          let e2', t2' = g env et2 in
          let e3', t3' = g env e3 in
          unify env t2' t3';
          If(set et1 (e1', t1'), set et2 (e2', t2'), set e3 (e3', t3')), t2'
      | Match(et, pets) ->
          let e', ty_e' = g env et in
          let pets', ts' = List.fold_left 
            (fun (pets, ts) (p, e) -> 
              let env', t' = pattern env p in
              unify env ty_e' t';
              let e', t' = g env' e in
              (p, set e (e', t')) :: pets, t' :: ts) ([], []) (List.rev pets) in
          let t1' = List.hd ts' in
          List.iter (unify env t1') (List.tl ts');
          Match(set et (e', ty_e'), pets'), t1'
      | LetVar((x, t), et1, et2) -> (* letの型推論 (caml2html: typing_let) *)
          let e1', t1' = g env et1 in
          let t1' = generalize env t1' in (* 副作用は未サポートなので、Tiger本のp.335にある代入の判定はなし *)
          unify env t t1';
          let e2', t2' = g (Env.add_var env x t1') et2 in
          LetVar((x, t1'), set et1 (e1', t1'), set et2 (e2', t2')), t2'
      | Var(x) when M.mem x venv -> 
          expr, instantiate env (M.find x venv) (* 変数の型推論 (caml2html: typing_var) *)
      | Var(x) ->
        raise (Syntax.Unbound_value_error (e.loc, x))
      | Constr(x, []) -> 
          expr, instantiate env (M.find x venv)
      | Constr(x, ets) -> 
        let ets', ts' =
          List.fold_left
            (fun (ets, ts) e ->
               let e', t' = g env e in
               set e (e', t') :: ets, t' :: ts)
            ([], []) (List.rev ets)
        in
        begin
          match instantiate env (M.find x venv) with
          | Type.App(Type.Arrow, ys) -> 
            List.iter2 (unify env) ts' (L.init ys);
            Constr(x, ets'), (L.last ys)
          | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t); assert false
        end
      | Module x when Module.mem x ->
        expr, instantiate env (Type.App(Type.Module x, []))
      | Module x ->
        raise (Syntax.Unbound_module_error (e.loc, x))
      | LetRec({ name = (x, ty_f); args = yts; body = et1 }, et2) -> (* let recの型推論 (caml2html: typing_letrec) *)
          let t2 = Type.Meta(Type.newmetavar()) in
          let ty_f' = Type.App(Type.Arrow, ((List.map snd yts) @ [t2])) in
          let e1', t1' = g { env with Env.venv = M.add_list yts (M.add x ty_f' venv) } et1 in
          unify env ty_f ty_f';
          unify env t2 t1';
          let t'' = generalize env ty_f' in
          let e2', t2' = g (Env.add_var env x t'') et2 in
          LetRec({ name = (x, t''); args = yts; body = set et1 (e1', t1') }, set et2 (e2', t2')), t2'
      | App(et, ets) -> (* 関数適用の型推論 (caml2html: typing_app) *)
          let e', t' = g env et in
          let ets', ts' =
            List.fold_left
              (fun (ets, ts) e ->
                 let e', t' = g env e in
                 set e (e', t') :: ets, t' :: ts)
              ([], []) (List.rev ets)
          in
          let result = Type.Meta(Type.newmetavar ()) in
          unify env t' (Type.App(Type.Arrow, ts' @ [result]));
          App(set et (e', t'), ets'), result
    in
    unify env ty ty';
    expr', ty'
  with Unify(t1, t2) -> 
    raise (Error(create e.loc & deref_expr env expr,
                 deref_type env t1, deref_type env t2))

let f' env (et, ty) = 
  try 
    let e', t' = g env et in
    unify env ty t';
    (e', t')
  with Unify _ -> failwith "type error."

let f defs = 
  
  let _, defs' = 
    List.fold_left (fun ({ Env.venv = venv; tenv = tenv; tycons = tycons; mods = mods } as env, defs) def ->
      let env', def' = 
        match def.desc with
        | TypeDef(x, t) -> 
            { Env.venv = M.add_list (Type.vars t) venv;
              Env.tenv = M.add_list (Type.types t) tenv;
              Env.tycons = M.add x t tycons;
              Env.mods = mods },
          TypeDef(x, t)
        | VarDef((x, t), et) -> 
            let et' = f' env (et, t) in
            Env.add_var env x t, VarDef((x, t), set et et')
        | RecDef({ name = (x, ty_f); args = yts; body = et }) -> 
            let ty_r = Type.Meta(Type.newmetavar()) in
            let ty_f' = Type.App(Type.Arrow, ((List.map snd yts) @ [ty_r])) in
            let et' = f' { env with Env.venv = M.add_list yts (M.add x ty_f' env.Env.venv) } (et, ty_r) in
            unify env ty_f ty_f';
            let t'' = (generalize env ty_f') in
            { env with Env.venv = M.add x t'' venv }, 
            RecDef({ name = (x, t''); 
                     args = yts;
                     body = set et et' })
        | _ -> assert false
      in
      env', (set def def') :: defs) ((Sig.create_env ()), []) defs
  in

  (* deref_def の中で未解決なメタ変数を型変数に置き換えてしまうので、すべての式の型推論が終わってから deref を呼ぶこと *)
  let { Env.venv = venv; tenv = tenv; tycons = tycons; mods = mods } as env = !Env.empty in
  Env.empty := { Env.venv = M.map (deref_type env) venv;
                 Env.tenv = M.map (deref_type env) tenv; 
                 Env.tycons = M.map (deref_tycon env) tycons;
                 Env.mods = mods };

  fold (fun (env, defs) def -> deref_def env def :: defs) (List.rev defs') (Sig.create_env ())
