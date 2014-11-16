(* type inference/reconstruction *)

open AstTypes
open Spotlib.Base

exception Unify of Type.t * Type.t
exception Error of t * Type.t * Type.t
exception Top_level_error of Type.t

let extenv = ref M.empty

(* for pretty printing (and type normalization) *)
let rec deref_typ = function (* 型変数を中身でおきかえる関数 (caml2html: typing_deref) *)
  | Type.Fun ({ fun_args = t1s; fun_ret = t2 } as f) ->
    Type.Fun { f with fun_args = List.map deref_typ t1s;
                      fun_ret = deref_typ t2 }
  | Type.Tuple(ts) -> Type.Tuple(List.map deref_typ ts)
  | Type.Array(t) -> Type.Array(deref_typ t)
  | Type.Var({ contents = None } as r) ->
    Log.debug "# uninstantiated type variable detected; assuming int\n";
    r := Some(Type.Int);
    Type.Int
  | Type.Var({ contents = Some(t) } as r) ->
    let t' = deref_typ t in
    r := Some(t');
    t'
  | t -> t

let deref_id_typ (x, t) = (x, deref_typ t)

let rec deref_arg_typ = function
  | FunArg.Var (x, t) -> FunArg.Var (x, deref_typ t)
  | FunArg.Tuple (id, es) -> FunArg.Tuple (id, List.map deref_arg_typ es)

let rec deref_term t =
  let f = deref_term in
  let desc = match Ast.desc t with
  | Not(e) -> Not(f e)
  | Neg(e) -> Neg(f e)
  | FNeg(e) -> FNeg(f e)
  | Bin (e1, op, e2) -> Bin (f e1, op, f e2)
  | If (e1, e2, e3) -> If (f e1, f e2, f e3)
  | Let(xt, e1, e2) -> Let(deref_id_typ xt, f e1, f e2)
  | Def { name = xt; rec_ = rec_; args = args; body = body } ->
    Def { name = deref_id_typ xt;
          rec_ = rec_;
          args = List.map deref_arg_typ args;
          body = f body }
  | App(e, es) -> App(f e, List.map f es)
  | Tuple(es) -> Tuple(List.map f es)
  | LetTuple(xts, e1, e2) -> LetTuple(List.map deref_id_typ xts, f e1, f e2)
  | Array(e1, e2) -> Array(f e1, f e2)
  | Get(e1, e2) -> Get(f e1, f e2)
  | Put(e1, e2, e3) -> Put(f e1, f e2, f e3)
  | e -> e
  in
  Location.with_loc (Ast.loc t) desc

(* occur check *)
let rec occur r1 = function
  | Type.Fun { fun_args = t2s; fun_ret = t2 } ->
    List.exists (occur r1) t2s || occur r1 t2
  | Type.Tuple(t2s) -> List.exists (occur r1) t2s
  | Type.Array(t2) -> occur r1 t2
  | Type.Var(r2) when r1 == r2 -> true
  | Type.Var({ contents = None }) -> false
  | Type.Var({ contents = Some(t2) }) -> occur r1 t2
  | _ -> false

let rec unify t1 t2 = (* 型が合うように、型変数への代入をする (caml2html: typing_unify) *)
  Log.debug "# unify: %s, %s\n" (Type.to_string t1) (Type.to_string t2);
  match t1, t2 with
  | Type.Unit, Type.Unit
  | Type.Bool, Type.Bool
  | Type.Int, Type.Int
  | Type.Float, Type.Float
  | Type.String, Type.String
    -> ()
  | Type.Fun { fun_args = t1s; fun_ret = t1' },
    Type.Fun { fun_args = t2s; fun_ret = t2' } ->
    (try List.iter2 unify t1s t2s
     with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)));
    unify t1' t2'
  | Type.Tuple(t1s), Type.Tuple(t2s) ->
      (try List.iter2 unify t1s t2s
      with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)))
  | Type.Array(t1), Type.Array(t2) -> unify t1 t2
  | Type.Var(r1), Type.Var(r2) when r1 == r2 -> ()
  | Type.Var({ contents = Some(t1') }), _ -> unify t1' t2
  | _, Type.Var({ contents = Some(t2') }) -> unify t1 t2'
  | Type.Var({ contents = None } as r1), _ -> (* 一方が未定義の型変数の場合 (caml2html: typing_undef) *)
      if occur r1 t2 then raise (Unify(t1, t2));
      r1 := Some(t2)
  | _, Type.Var({ contents = None } as r2) ->
      if occur r2 t1 then raise (Unify(t1, t2));
      r2 := Some(t1)
  | _, _ ->
    Log.debug "# unify error: %s, %s\n" (Type.to_string t1) (Type.to_string t2);
    raise (Unify(t1, t2))

let binop_typ = function
  | BinOp.Add -> Type.Int
  | BinOp.Sub -> Type.Int
  | BinOp.FAdd -> Type.Float
  | BinOp.FSub-> Type.Float
  | BinOp.FMul -> Type.Float
  | BinOp.FDiv -> Type.Float
  | BinOp.SConcat -> Type.String
  | op -> failwith (Printf.sprintf "binop_typ %s" (BinOp.to_string op))

let rec g env e = (* 型推論ルーチン (caml2html: typing_g) *)
  try
    match Ast.desc e with
    | Nop -> Type.Unit (* TODO: error? *)
    | Unit -> Type.Unit
    | Bool(_) -> Type.Bool
    | Int(_) -> Type.Int
    | Float(_) -> Type.Float
    | String _ -> Type.String
    | Typed (e, t) ->
      begin match e.desc with
      | List [] -> t
      | _ ->
        unify t (g env e);
        t
      end
    | Not(e) ->
	unify Type.Bool (g env e);
	Type.Bool
    | Neg(e) ->
	unify Type.Int (g env e);
	Type.Int
    | FNeg(e) ->
	unify Type.Float (g env e);
	Type.Float
    | Bin (e1, op, e2) ->
      begin match op with
      | BinOp.Eq
      | BinOp.LE
      | BinOp.LT
      | BinOp.GE
      | BinOp.GT ->
	    unify (g env e1) (g env e2);
	    Type.Bool
      | _ ->
        let t = binop_typ op in
        unify t (g env e1);
        unify t (g env e2);
        t
      end
    | If(e1, e2, e3) ->
      unify (g env e1) Type.Bool;
      let t2 = g env e2 in
      let t3 = g env e3 in
      unify t2 t3;
      t2
    | Let((x, t), e1, e2) -> (* letの型推論 (caml2html: typing_let) *)
	unify t (g env e1);
	g (M.add x t env) e2
    | Var(x) when M.mem x env -> M.find x env (* 変数の型推論 (caml2html: typing_var) *)
    | Var(x) when M.mem x !extenv -> M.find x !extenv
    | Var(x) -> (* 外部変数の型推論 (caml2html: typing_extvar) *)
      Log.debug "# free variable \"%s\" assumed as external.\n" x;
      M.iter (fun k _ -> Log.debug "# env key = \"%s\"\n" k) env;
      begin match Context.current_module () with
      | None -> assert false
      | Some m ->
        match Module.find_opt m x with
        | None ->
          raise (Type.Parse_error
                 (e.loc, Printf.sprintf "`%s' is not defined" x))
        | Some f ->
          let t = Fun.to_typ f in
          extenv := M.add x t !extenv;
          t
      end

    | Local (x, e) ->
      begin match Context.find_module_opt x with
      | None ->
        raise (Type.Parse_error
                 (e.loc, Printf.sprintf "module `%s' is not found or unbound" x))
      | Some m -> g (M.add_list (Module.fun_typs m) env) e
      end
    | Def { name = (x, t); args = yts; body = body } ->
      let env' = M.add x t env in
      let ret = g (M.add_list (FunArg.vars yts) env') body in
      unify t Fun.(to_typ & create (List.map FunArg.typ yts) ret);
      Type.Unit

    | App(e, es) -> (* 関数適用の型推論 (caml2html: typing_app) *)
      let t = Type.gentyp () in
      unify (g env e) Fun.(to_typ & create (List.map (g env) es) t);
      t
    | Tuple(es) -> Type.Tuple(List.map (g env) es)
    | LetTuple(xts, e1, e2) ->
	unify (Type.Tuple(List.map snd xts)) (g env e1);
	g (M.add_list xts env) e2
    | List [] ->
      raise (Type.Parse_error (e.loc, "empty list without type specification is not yet supported."))
    | List es ->
      let t = g env & List.hd es in
      List.iter (fun e -> unify t & g env e) (List.tl es);
      Type.List t
    | Array(e1, e2) -> (* must be a primitive for "polymorphic" typing *)
	unify (g env e1) Type.Int;
	Type.Array(g env e2)
    | Get(e1, e2) ->
	let t = Type.gentyp () in
	unify (Type.Array(t)) (g env e1);
	unify Type.Int (g env e2);
	t
    | Put(e1, e2, e3) ->
	let t = g env e3 in
	unify (Type.Array(t)) (g env e1);
	unify Type.Int (g env e2);
	Type.Unit
    | SigDef _ -> failwith "sigdef"
  with Unify(t1, t2) ->
    Printf.printf "%d:%d-%d: infer error: %s\n"
      Location.(e.loc.start.line)
      Location.(e.loc.start.col)
      Location.(e.loc.end_.col)
      (Ast.to_string e);
    raise (Error(deref_term e, deref_typ t1, deref_typ t2))

let f e =
  extenv := M.empty;
  begin try unify Type.Unit (g (Env.create ()) e) with
  | Unify (_, t) -> raise (Top_level_error t)
  end;
  extenv := M.map deref_typ !extenv;
  deref_term e
