(* give names to intermediate values (K-normalization) *)
open Spotlib
open Base

module L = Location

type t = (* K正規化後の式 (caml2html: knormal_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | SConcat of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t (* 比較 + 分岐 (caml2html: knormal_branch) *)
  | IfLE of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | Mod_fun of Fun.t
  | Def of fundef
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | List of Id.t list
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
and fundef = {
  name : Id.t * Type.t;
  rec_ : bool;
  args : FunArg.t list;
  body : t
}

let rec to_string t =
  let open Printf in
  match t with
  | Unit -> "Unit"
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> sprintf "\"%s\"" s
  | Neg x -> sprintf "(- %s)" x
  | FNeg x -> sprintf "(-. %s)" x
  | Add (x, y) -> sprintf "(%s + %s)" x y
  | Sub (x, y) -> sprintf "(%s - %s)" x y
  | FAdd (x, y) -> sprintf "(%s +. %s)" x y
  | FSub (x, y) -> sprintf "(%s -. %s)" x y
  | FMul (x, y) -> sprintf "(%s *. %s)" x y
  | FDiv (x, y) -> sprintf "(%s /. %s)" x y
  | SConcat (x, y) -> sprintf "(SConcat %s %s)" x y
  | IfEq (x, y, e1, e2) ->
    sprintf "(IfEq %s %s %s %s)" x y (to_string e1) (to_string e2)
  | IfLE (x, y, e1, e2) ->
    sprintf "(IfLE %s %s %s %s)" x y (to_string e1) (to_string e2)
  | Let ((x, t), e1, e2) ->
    sprintf "(Let (%s, %s) %s %s)" x
      (Type.to_string t) (to_string e1) (to_string e2)
  | Var x -> sprintf "$%s" x
  | Def { name = (x, t); rec_ = rec_ } ->
    sprintf "(Def %s%s:%s)" (if rec_ then "rec " else "")
      x (Type.to_string t)
  | Mod_fun f -> Fun.to_string f
  | App (x, ys) -> sprintf "(App %s %s)" x (String.concat " " ys)
  | Tuple xs -> sprintf "(%s)" (String.concat ", " xs)
  | List xs -> sprintf "[%s]" (String.concat ", " xs)
             (*
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
              *)
  | _ -> sprintf "unknown"

let rec fv = function (* 式に出現する（自由な）変数 (caml2html: knormal_fv) *)
  | Unit | Bool _ | Int(_) | Float(_) | String _ | ExtArray(_) | Mod_fun _ -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | SConcat(x, y) | Get(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | Def { name = (x, t); args = yts; body = e1 } ->
    S.diff (fv e1) (S.of_list (FunArg.names yts))
  | App(x, ys) -> S.of_list (x :: ys)
  | Tuple(xs) | List xs | ExtFunApp(_, xs) -> S.of_list xs
  | Put(x, y, z) -> S.of_list [x; y; z]
  | LetTuple(xs, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xs)))

let insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e with
  | Var(x) -> k x
  | _ ->
      let x = Id.gentmp t in
      let e', t' = k x in
      Let((x, t), e, e'), t'

(* K正規化ルーチン本体 (caml2html: knormal_g) *)
let rec g env e =
  match Ast.desc e with
  | AstTypes.Nop -> failwith "nop"
  | AstTypes.SigDef _ -> failwith "sigdef"
  | AstTypes.Unit -> Unit, Type.Unit
  (*| AstTypes.Bool(b) -> Int(if b then 1 else 0), Type.Int (* 論理値true, falseを整数1, 0に変換 (caml2html: knormal_bool) *)*)
  | AstTypes.Bool(b) -> Bool(b), Type.Bool
  | AstTypes.Int(i) -> Int(i), Type.Int
  | AstTypes.Float(d) -> Float(d), Type.Float
  | AstTypes.String s -> String s, Type.String
  | AstTypes.Not(e) ->
    g env & L.replace e (fun l _ -> 
      (AstTypes.If (e, L.with_loc l (AstTypes.Bool false), L.with_loc l (AstTypes.Bool true))))
  | AstTypes.Neg(e) ->
      insert_let (g env e)
	(fun x -> Neg(x), Type.Int)
  | AstTypes.FNeg(e) ->
      insert_let (g env e)
	(fun x -> FNeg(x), Type.Float)

  | AstTypes.Bin (e1, op, e2) ->
    begin match op with
    | BinOp.Add ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
          (fun y -> Add (x, y), Type.Int))
    | BinOp.Sub ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
          (fun y -> Sub (x, y), Type.Int))
    | BinOp.FAdd ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
          (fun y -> FAdd (x, y), Type.Float))
    | BinOp.FSub ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
          (fun y -> FSub (x, y), Type.Float))
    | BinOp.FMul ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
          (fun y -> FMul (x, y), Type.Float))
    | BinOp.FDiv ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
	      (fun y -> FDiv (x, y), Type.Float))
    | BinOp.SConcat ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
	      (fun y -> SConcat (x, y), Type.String))
    | _ -> (* cmp *)
      g env & L.replace e
        (fun l _ -> (AstTypes.If (e, L.with_loc l (AstTypes.Bool true),
                         L.with_loc l (AstTypes.Bool false))))
    end
  | AstTypes.If({L.desc = AstTypes.Not(e1)}, e2, e3) ->
    (* notによる分岐を変換 (caml2html: knormal_not) *)
    g env & L.replace e (fun l _ -> AstTypes.If (e1, e3, e2))
  | AstTypes.If({L.desc = AstTypes.Bin (e1, BinOp.Eq, e2)}, e3, e4) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
      (fun y ->
        let e3', t3 = g env e3 in
        let e4', t4 = g env e4 in
        IfEq(x, y, e3', e4'), t3))
  | AstTypes.If({L.desc = AstTypes.Bin (e1, BinOp.LE, e2)}, e3, e4) ->
    insert_let (g env e1)
      (fun x -> insert_let (g env e2)
      (fun y ->
        let e3', t3 = g env e3 in
        let e4', t4 = g env e4 in
        IfLE(x, y, e3', e4'), t3))
  | AstTypes.If (e1, e2, e3) ->
      (* TODO: 他のオペレータに対応できてない *)
    (* 比較のない分岐を変換 (caml2html: knormal_if) *)
    let l = Ast.loc e1 in
    L.(g env & with_loc l
                (AstTypes.If (with_loc l (AstTypes.Bin (e1, BinOp.Eq, with_loc l (AstTypes.Bool false))), e3, e2)))
  | AstTypes.Let((x, t), e1, e2) ->
    let e1', t1 = g env e1 in
    let e2', t2 = g (M.add x t env) e2 in
    Let((x, t), e1', e2'), t2
  | AstTypes.Var(x) when M.mem x env ->
    begin match M.find x env with
    | Type.Fun ({ Type.fun_mod = Some _; fun_name = Some _ } as f) as t ->
      Mod_fun(f), t
    | t -> Var(x), t
    end
  | AstTypes.Var(x) when Context.mem_top_typ x ->
    Var(x), Fun.to_typ (Context.find_top_typ x)
  | AstTypes.Var(x) -> (* 外部配列の参照 (caml2html: knormal_extarray) *)
    Log.debug "# external array `%s'\n" x;
      (match M.find x !Typing.extenv with
      | Type.Array(_) as t -> ExtArray x, t
      | _ -> failwith (Printf.sprintf "external variable %s does not have an array type" x))
  | AstTypes.Local (x, e) ->
    begin match Context.find_module_opt x with
    | None -> assert false
    | Some m -> g (M.add_list (Module.fun_typs m) env) e
    end
                    (*
  | AstTypes.LetRec({ AstTypes.name = (x, t); AstTypes.args = yts; AstTypes.body = e1 }, e2) ->
      let env' = M.add x t env in
      let e2', t2 = g env' e2 in
      let e1', t1 = g (M.add_list yts env') e1 in
      LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
                     *)
  | AstTypes.Def { AstTypes.name = (x, t); AstTypes.rec_ = rec_; AstTypes.args = yts; AstTypes.body = e1 } ->
    let env' = M.add x t env in
    let e1', t1 = g (M.add_list (FunArg.vars yts) env') e1 in
    (* FIXME: t1 (return type) is not correct. *)
    Context.add_top_typ x (Fun.create [Type.Unit] t1); (* TODO: args *)
    Def { name = (x, t); rec_ = rec_; args = yts; body = e1' }, t1
  | AstTypes.App({L.desc = AstTypes.Var(f)}, e2s)
    when not (M.mem f env) && not (Context.mem_top_typ f) -> (* 外部関数の呼び出し (caml2html: knormal_extfunapp) *)
      (* TODO: error? *)
      Log.debug "# kNormal: call extfun %s\n" f;
      (match M.find f !Typing.extenv with
      | Type.Fun { fun_ret = t } ->
	  let rec bind xs = function (* "xs" are identifiers for the arguments *)
	    | [] -> ExtFunApp(f, xs), t
	    | e2 :: e2s ->
		insert_let (g env e2)
		  (fun x -> bind (xs @ [x]) e2s) in
	  bind [] e2s (* left-to-right evaluation *)
      | _ -> assert false)
  | AstTypes.App(e1, e2s) ->
    let (t, g_e1) =
      match g env e1 with
      | _, Type.Fun { fun_ret = t } as g_e1 -> (t, g_e1)
      | _ -> assert false
    in
    insert_let g_e1
    (fun f ->
      let rec bind xs = function (* "xs" are identifiers for the arguments *)
        | [] -> App(f, xs), t
        | e2 :: e2s ->
          insert_let (g env e2)
            (fun x -> bind (xs @ [x]) e2s)
      in
      bind [] e2s) (* left-to-right evaluation *)
  | AstTypes.Tuple(es) ->
      let rec bind xs ts = function (* "xs" and "ts" are identifiers and types for the elements *)
	| [] -> Tuple(xs), Type.Tuple(ts)
	| e :: es ->
	    let _, t as g_e = g env e in
	    insert_let g_e
	      (fun x -> bind (xs @ [x]) (ts @ [t]) es) in
      bind [] [] es
  | AstTypes.LetTuple(xts, e1, e2) ->
      insert_let (g env e1)
	(fun y ->
	  let e2', t2 = g (M.add_list xts env) e2 in
	  LetTuple(xts, y, e2'), t2)
  | AstTypes.List es ->
    let rec bind xs t = function (* "xs" and "ts" are identifiers and types for the elements *)
      | [] -> (List xs, Type.List t)
      | e :: es ->
        let _, t as g_e = g env e in
        insert_let g_e (fun x -> bind (xs @ [x]) t es)
    in
    bind [] Unit es
  | AstTypes.Typed (e, t) ->
    begin match e.desc with
    | AstTypes.List [] -> (List [], t)
    | _ -> g env e
    end
  | AstTypes.Array(e1, e2) ->
      insert_let (g env e1)
	(fun x ->
	  let _, t2 as g_e2 = g env e2 in
	  insert_let g_e2
	    (fun y ->
	      let l =
		match t2 with
		| Type.Float -> "create_float_array"
		| _ -> "create_array" in
	      ExtFunApp(l, [x; y]), Type.Array(t2)))
  | AstTypes.Get(e1, e2) ->
      (match g env e1 with
      |	_, Type.Array(t) as g_e1 ->
	  insert_let g_e1
	    (fun x -> insert_let (g env e2)
		(fun y -> Get(x, y), t))
      | _ -> assert false)
  | AstTypes.Put(e1, e2, e3) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> insert_let (g env e3)
		(fun z -> Put(x, y, z), Type.Unit)))

let f e = fst (g (Env.create ()) e)
