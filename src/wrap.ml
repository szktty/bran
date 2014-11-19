(* 多相型をラップするモジュール *)

open Syntax

let wrappers = Hashtbl.create 64
let unwrappers = Hashtbl.create 64
let wrapper_defs = ref []
    
let gen_wrapper tv ty = 
  let x, ty' = 
    match ty with
    | Type.App(Type.Bool, []) 
    | Type.App(Type.Int, []) 
    | Type.App(Type.Record(_, _), _) 
    | Type.App(Type.Tuple, _) -> 
        "wrap_" ^ (Type.name ty), ty
    | Type.App(Type.Arrow, us) -> 
        (Id.genid "wrap_closure"), (Type.App(Type.Arrow, (List.map (fun _ -> (Type.Var(Type.newtyvar ()))) us)))
    | Type.App(Type.Variant(x, _), _) ->
        "wrap_" ^ x, ty
    | ty -> Printf.eprintf "not implemented. ty = %s\n" (Type.string_of_t ty); assert false in
  let ty_f = Type.App(Type.Arrow, [ty'; tv]) in  
  let y = Id.gentmp (Type.prefix ty') in  
  (x, ty_f), RecDef({ name = (x, ty_f); args = [(y, ty')]; body = WrapBody(y, ty'), ty })
    
let gen_unwrapper tv ty = 
  let x, ty = 
    match ty with
    | Type.App(Type.Bool, []) 
    | Type.App(Type.Int, []) 
    | Type.App(Type.Record(_, _), _) 
    | Type.App(Type.Tuple, _) -> 
        "unwrap_" ^ (Type.name ty), ty
    | Type.App(Type.Arrow, vs) ->
        (Id.genid "unwrap_closure"), (Type.App(Type.Arrow, (List.map (fun _ -> Type.Var(Type.newtyvar ())) vs)))
    | Type.App(Type.Variant(x, _), _) ->
        "unwrap_" ^ x, ty
    | ty -> Printf.eprintf "not implemented. ty = %s\n" (Type.string_of_t ty); assert false in
  let ty_f = Type.App(Type.Arrow, [tv; ty]) in  
  let y = Id.gentmp (Type.prefix tv) in  
  (x, ty_f), RecDef({ name = (x, ty_f); args = [(y, tv)]; body = UnwrapBody(y, ty), ty })

let find_wrapper env table tv ty generator =
  let generalize env =
    function
    | Type.App(Type.Variant(x, _), _) when Env.exists_tycon env x -> 
        begin 
          match Env.find_tycon env x with
          | Type.TyFun(_, (Type.App(Type.Variant(_, _), _) as t)) -> t
          | t -> Printf.eprintf "invalid type constructor : %s\n" (Type.string_of_tycon t); assert false
        end
    | ty -> ty in
  let ty = generalize env ty in
  try Hashtbl.find table ty with
    Not_found ->
      let (x, ty_f), def = generator tv ty in
      let expr = Var(x), ty_f in
      Hashtbl.add table ty expr;
      wrapper_defs := def :: !wrapper_defs;
      expr
        
let wrapper env ty tv = find_wrapper env wrappers tv ty gen_wrapper
let unwrapper env tv ty = find_wrapper env unwrappers tv ty gen_unwrapper

let rec has_tyvar t = 
  match t with
  | Type.Var _ -> true
  | Type.Field(_, s) -> has_tyvar s
  | Type.App(Type.Variant(_, constrs), us) -> (List.exists (fun (_, us) -> (List.exists has_tyvar us)) constrs) || (List.exists has_tyvar us)
  | Type.App(_, us) -> List.exists has_tyvar us
  | Type.Poly(_, s) -> assert false
  | Type.Meta({ contents = None }) -> false
  | Type.Meta({ contents = Some(t') }) -> has_tyvar t'
      
(* 関数適応時の引数の包み込み。Tiger本の p.345 *)
let rec wrap env (expr, ty_src) ty_dst = 
  let _ = Log.debug "Wrap.wrap \n  (expr = %s,\n   ty_src = %s)\n  ,ty_dst = %s\n" (string_of_expr expr) (Type.string_of_t ty_src) (Type.string_of_t ty_dst) in
  let expr', ty' =
    match ty_src, ty_dst with
    | s, t when Type.equal s t -> expr, t
    | Type.Field(_, s), t
    | s, Type.Field(_, t) -> wrap env (expr, s) t
    | Type.Var _, Type.Var _ -> expr, ty_dst
    | Type.App(Type.Bool, [])      , Type.Var _ 
    | Type.App(Type.Int, [])       , Type.Var _ 
    | Type.App(Type.Record _, _)   , Type.Var _ 
    | Type.App(Type.Tuple, _)      , Type.Var _ 
    | Type.App(Type.Variant(_), _) , Type.Var _ ->
        App(wrapper env ty_src ty_dst, [(expr, ty_src)]), ty_dst
    | Type.App(Type.Arrow, us), Type.Var _ -> 
        let name = Id.genid (match expr with Var(x) -> "wrap_" ^ x | _ -> "wrap_fun") in
        let yts = List.map (fun u -> (Id.gentmp (Type.prefix u), Type.Var(Type.newtyvar ()))) (L.init us) in
        let ty_r = Type.Var(Type.newtyvar ()) in
        let e' = 
          App(wrapper env ty_src ty_dst, 
              let ty_f = Type.App(Type.Arrow, (List.map snd yts) @ [ty_r]) in
              [(LetRec({ name = (name, ty_f); 
                         args = yts;
                         body = wrap env (App((expr, ty_src), List.map2 (fun (y, t) u -> unwrap env (Var(y), t) u) yts (L.init us)), (L.last us)) ty_r },
                       (Var(name), ty_f)), ty_f)]) in
        e', ty_dst
    | Type.App(Type.Arrow, us), Type.App(Type.Arrow, vs) when has_tyvar ty_dst ->
        let name = Id.genid (match expr with Var(x) -> "wrap_" ^ x | _ -> "wrap_fun") in
        let yts = List.map (fun v -> (Id.gentmp (Type.prefix v), v)) (L.init vs) in
        begin
          match (L.last vs) with
          | Type.Var _ -> 
              let ty_f = Type.App(Type.Arrow, (List.map snd yts) @ [L.last vs]) in
              LetRec({ name = (name, ty_f); 
                       args = yts;
                       body = wrap env (App((expr, ty_src), List.map2 (fun (y, t) u -> unwrap env (Var(y), t) u) yts (L.init us)), (L.last us)) (L.last vs) },
                     (Var(name), ty_f)), ty_f
          | r -> 
              let ty_f = Type.App(Type.Arrow, (List.map snd yts) @ [r]) in
              LetRec({ name = (name, ty_f); 
                       args = yts;
                       body = unwrap env (App((expr, ty_src), List.map2 (fun (y, t) u -> unwrap env (Var(y), t) u) yts (L.init us)), (L.last us)) r }, 
                     (Var(name), ty_f)), ty_f
        end
    | s, t -> Printf.eprintf "not implemented. \ns = %s\nt = %s\n" (Type.string_of_t s) (Type.string_of_t t); assert false in
  expr', ty'
    
and unwrap env (expr, ty_src) ty_dst = 
  let _ = Log.debug "Wrap.unwrap \n  (expr = %s,\n   s = %s)\n  ,t = %s\n" (string_of_expr expr) (Type.string_of_t ty_src) (Type.string_of_t ty_dst) in
  let expr', ty' =
    match ty_src, ty_dst with
    | s, t when Type.equal s t -> expr, t
    | Type.App(Type.Variant(x, _), _), Type.App(Type.Variant(y, _), _) when x = y -> expr, ty_dst
    | Type.Var _, Type.Var _ -> expr, ty_dst
    | Type.Var _, Type.App(Type.Bool, []) 
    | Type.Var _, Type.App(Type.Int, []) 
    | Type.Var _, Type.App(Type.Record _, _) 
    | Type.Var _, Type.App(Type.Tuple, _) 
    | Type.Var _, Type.App(Type.Variant(_), _) ->
        App(unwrapper env ty_src ty_dst, [(expr, ty_src)]), ty_dst
    | Type.Var _, Type.App(Type.Arrow, vs) ->
        let e' = 
        let name = Id.genid (match expr with Var(x) -> "unwrap_" ^ x | _ -> "unwrap_fun") in
          let yts = List.map (fun v -> (Id.gentmp (Type.prefix v), v)) (L.init vs) in
          LetRec({ name = (name, ty_dst);
                   args = yts;
                   body = let t' = List.map (fun _ -> Type.Var(Type.newtyvar ())) vs in
                          let e' = App(unwrapper env ty_src (Type.App(Type.Arrow, t')), [(expr, ty_src)]), (Type.App(Type.Arrow, t')) in
                          unwrap env (App(e', (List.map2 (fun (y, t) t' -> wrap env (Var(y), t) t') yts (L.init t'))), (L.last t')) (L.last vs) },
                 (Var(name), ty_dst)) in
        e', ty_dst
    | Type.App(Type.Arrow, us), Type.App(Type.Arrow, vs) when has_tyvar ty_src ->
        let name = Id.genid (match expr with Var(x) -> "unwrap_" ^ x | _ -> "unwrap_fun") in
        let yts = List.map (fun v -> (Id.gentmp (Type.prefix v), v)) (L.init vs) in
        LetRec({ name = (name, ty_dst); 
                 args = yts;
                 body = unwrap env (App((expr, ty_src), List.map2 (fun (y, t) u -> wrap env (Var(y), t) u) yts (L.init us)), (L.last us)) (L.last vs) },
               (Var(name), ty_dst)), ty_dst
    | _ -> Printf.eprintf "not implemented.\n"; assert false
  in
  expr', ty'
 
let subst_map s t =
  let _ = Log.debug "Wrap.subst_map %s %s\n" (Type.string_of_t s) (Type.string_of_t t) in
  let rec loop s t xs =
    match s, t with 
    | Type.Var(v), _ -> (v, t) :: xs
    | Type.App(Type.TyFun(_, s'), us), Type.App(Type.TyFun(_, t'), vs) -> 
        let xs' = loop s t xs in
        List.fold_left2 (fun xs u v -> loop u v xs) xs' us vs
    | Type.App(_, us), Type.App(_, vs) ->
        List.fold_left2 (fun xs u v -> loop u v xs) xs us vs
    | s, t -> xs in (* Printf.eprintf "invalid type : s = %s\n  t = %s\n" (Type.string_of_typ s) (Type.string_of_typ t); assert false in *)
  loop s t []

let rec pattern ({ Env.venv = venv; tenv = tenv } as env) p =    
  match p with
  | PtBool(b) -> env, Type.App(Type.Bool, [])
  | PtInt(n) -> env, Type.App(Type.Int, [])
  | PtVar(x, t) -> Env.add_var env x t, t
  | PtTuple(ps) -> 
      let env, _, ts' = List.fold_left (fun (env, ps, ts) p -> let env', t' = pattern env p in env', p :: ps, t' :: ts) (env, [], []) (List.rev ps) in
      env, Type.App(Type.Tuple, ts')
  | PtRecord(xps) -> 
      let env, _, ts' = List.fold_left (fun (env, xps, ts) (x, p) -> let env', t' = pattern env p in env', (x, p) :: xps, t' :: ts) (env, [], []) (List.rev xps) in
      begin
        match M.find (fst (List.hd xps)) tenv with
        | Type.Poly(_, Type.Field(t, _)) ->
            begin 
              match t with
              | Type.App(Type.Record(x, tys), _) ->
                  env, Type.App(Type.Record(x, tys), ts')
              | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t); assert false
            end
        | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t); assert false
      end
  | PtConstr(x, ps) -> 
      let env, _, ts' = List.fold_left (fun (env, ps, ts) p -> let env', t' = pattern env p in env', p :: ps, t' :: ts) (env, [], []) (List.rev ps) in
      begin
        match M.find x venv with
        | Type.Poly(_, (Type.App(Type.Variant _, _) as t)) ->
            assert (ps = []);
            env, t
        | Type.Poly(_, Type.App(Type.Arrow, _)) ->
            env, Type.App(Type.Arrow, ts')
        | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t); assert false
      end

let instantiate =
  function
  | Type.Poly(_, t) -> t
  | t -> t

let rec g ({ Env.venv = venv; tenv = tenv } as env) (expr, ty) =
  let _ = Log.debug "Wrap.g %s\n" (string_of_typed_expr (expr, ty)) in

  let unary et f =
    let et' = g env et in
    f et' in

  let binop et1 et2 f =
    let et1' = g env et1 in
    let et2' = g env et2 in
    f et1' et2' in

  let expr', ty' = 
    match expr with
    | Unit | Bool _ | Int _ -> expr, ty
    | Record(xets) -> 
        let xets', ts' = List.fold_left (fun (xets, ts) (x, e) -> let e', t' = g env e in (x, (e', t')) :: xets, t' :: ts) ([], []) (List.rev xets) in 
        begin
          match M.find (fst (List.hd xets)) tenv with
          | Type.Poly(xs, Type.Field(ty_rec, _)) -> 
              begin 
                match ty_rec with 
                | Type.App(Type.Record(_, _), tys) ->
                    let xets' = List.map2 (fun (x, et') t -> x, wrap env et' t) xets' tys in
                    (Record(xets'), ty_rec)
                | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t); assert false
              end
          | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t); assert false
        end
    | Field(et, x) -> 
        let et' = g env et in       
        (match M.find x tenv with
        | Type.Poly(_, Type.Field(_, t')) -> 
            (unwrap env (Field(et', x), t') ty)
        | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t); assert false)
    | Tuple(ets) -> 
        let ets' = List.map (g env) ets in
        Tuple(ets'), Type.App(Type.Tuple, List.map snd ets')
    | Not(et) -> unary et (fun et' -> Not(et'), ty)
    | Neg(et) -> unary et (fun et' -> Neg(et'), ty)
    | And(et1, et2) -> binop et1 et2 (fun et1' et2' -> And(et1', et2'), ty)
    | Or(et1, et2)  -> binop et1 et2 (fun et1' et2' -> Or(et1', et2'), ty)
    | Add(et1, et2) -> binop et1 et2 (fun et1' et2' -> Add(et1', et2'), ty)
    | Sub(et1, et2) -> binop et1 et2 (fun et1' et2' -> Sub(et1', et2'), ty)
    | Mul(et1, et2) -> binop et1 et2 (fun et1' et2' -> Mul(et1', et2'), ty)
    | Div(et1, et2) -> binop et1 et2 (fun et1' et2' -> Div(et1', et2'), ty)
    | Eq(et1, et2)  -> binop et1 et2 (fun et1' et2' -> Eq(et1', et2'), ty)
    | LE(et1, et2)  -> binop et1 et2 (fun et1' et2' -> LE(et1', et2'), ty)
    | If(et1, (e1, ty2 as et2), (e3, ty3 as et3)) -> 
        let _, ty1' as et1' = g env et1 in
        let _, ty2' as et2' = g env et2 in
        let _, ty3' as et3' = g env et3 in
        if (Type.equal ty2' ty3') then
        If(et1', et2', et3'), ty2'
        else 
        If(et1', unwrap env et2' ty2, unwrap env et3' ty3), ty
    | Match(et, pets) -> 
        let et' = g env et in
        let pets' = List.map (fun (p, et) -> p, g (fst (pattern env p)) et) pets in
        let (_, (_, ty1)) = List.hd pets' in
        if (List.for_all (fun (_, (_, t)) -> Type.equal t ty1) (List.tl pets')) then
        Match(et', pets'), ty1
        else
        Match(et', List.map2 (fun (_, (_, t)) (p', e') -> p', unwrap env e' t) pets pets'), ty
    | LetVar((x, t), et1, et2) -> 
        let et1' = g env et1 in
        let _, ty2' as et2' = g (Env.add_var env x t) et2 in
        LetVar((x, t), et1', et2'), ty2'
    | Var(x) when M.mem x !Env.extenv.Env.venv -> Var(x), instantiate (M.find x !Env.extenv.Env.venv)
    | Var(x) -> Var(x), instantiate (M.find x venv)
    | Constr(x, []) -> Constr(x, []), ty
    | Constr(x, ets) -> 
        let ets' = List.map (g env) ets in
        begin
          match instantiate (M.find x venv) with
          | Type.App(Type.Arrow, tys) as ty_f -> 
              let ty_f = Typing.subst env (List.fold_left2 (fun tyvars s (_, t) -> M.add_list (subst_map s t) tyvars) M.empty (L.init tys) ets') ty_f in
              let ty_r = match ty_f with Type.App(Type.Arrow, tys') -> L.last tys' | _ -> assert false in
              (unwrap env (Constr(x, List.map2 (wrap env) ets' (L.init tys)), (L.last tys)) ty_r)
          | t -> Printf.eprintf "invalid type : t = %s\n" (Type.string_of_t t); assert false
        end
    | LetRec({ name = (x, ty_f); args = yts; body = et1 }, et2) -> 
        let et1' = g { env with Env.venv = M.add_list yts (M.add x ty_f venv) } et1 in
        let _, ty2' as et2' = g (Env.add_var env x ty_f) et2 in
        LetRec({ name = (x, ty_f); args = yts; body = et1' }, et2'), ty2'
    | App(et, ets) -> 
        let _, ty' as et' = g env et in
        let ets' = List.map (g env) ets in 
        (match ty' with 
        | Type.App(Type.Arrow, tys) ->
            let ty_f = Typing.subst env (List.fold_left2 (fun env s (_, t) -> M.add_list (subst_map s t) env) M.empty (L.init tys) ets') (Type.App(Type.Arrow, tys)) in
            let ty_r = match ty_f with Type.App(Type.Arrow, tys) -> L.last tys | _ -> assert false in
            (unwrap env (App(et', List.map2 (wrap env) ets' (L.init tys)), (L.last tys)) ty_r)
        | t -> Printf.eprintf "invalid type : %s\n" (Type.string_of_t t); assert false)
    | WrapBody _ | UnwrapBody _ -> Printf.eprintf "impossible.\n"; assert false 
  in
  expr', ty'
    
let f' env et = g env et
    
let f defs = 

  let () =
    let setup_predef table generator ty = 
      let (x, ty_f), _ = generator (Type.Var(Type.newtyvar ())) ty in
      Env.extenv := { !Env.extenv with Env.venv = M.add x ty_f !Env.extenv.Env.venv };
      Hashtbl.add table ty (Var(x), ty_f) in
    
    let setup_predef_wrapper = setup_predef wrappers gen_wrapper in
    let setup_predef_unwrapper = setup_predef unwrappers gen_unwrapper in
    
    List.iter 
      (fun ty -> 
        setup_predef_wrapper ty; 
        setup_predef_unwrapper ty)
      [Type.App(Type.Int, []); Type.App(Type.Bool, [])] in

  fold (fun (env, defs) def -> 
    match def with
    | TypeDef(x, t) -> 
        TypeDef(x, t) :: defs
    | VarDef((x, t), e) -> 
        let e' = f' env e in
        let defs' = !wrapper_defs @ defs in
        wrapper_defs := [];
        VarDef((x, t), e') :: defs'
    | RecDef({ name = (x, t); args = yts; body = e }) -> 
        let e' = f' env e in
        let defs' = !wrapper_defs @ defs in
        wrapper_defs := [];
        RecDef({ name = (x, t); args = yts; body = e' }) :: defs')
    defs
