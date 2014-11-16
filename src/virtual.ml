(* translation into assembly with infinite number of virtual registers *)

let data = ref [] (* 浮動小数点数の定数テーブル (caml2html: virtual_data) *)
let sdata = ref [] (* 文字列用 *)

let classify xts ini addf addi =
  List.fold_left
    (fun acc (x, t) ->
      match t with
      | Type.Unit -> acc
      | Type.Float -> addf acc x
      | _ -> addi acc x t)
    ini
    xts

let separate xts =
  classify
    xts
    ([], [])
    (fun (int, float) x -> (int, float @ [x]))
    (fun (int, float) x _ -> (int @ [x], float))

let expand xts ini addf addi =
  classify
    xts
    ini
    (fun (offset, acc) x ->
      let offset = Asm.align offset in
      (offset + 8, addf x offset acc))
    (fun (offset, acc) x t ->
      (offset + 4, addi x t offset acc))

let rec g env e = (* 式の仮想マシンコード生成 (caml2html: virtual_g) *)
  let open Asm in
  match e with
  | Closure.Unit -> Ans(Nop)
  | Closure.Bool b -> Ans(SetB(b))
  | Closure.Int(i) -> Ans(Set(i))
  | Closure.Float(d) ->
    let l =
	try
	  (* すでに定数テーブルにあったら再利用 *)
	  let (l, _) = List.find (fun (_, d') -> d = d') !data in
	  l
	with Not_found ->
	  let l = Id.L(Id.genid "L") in
	  data := (l, d) :: !data;
	  l in
      let x = Id.genid "L" in
      Let((x, Type.Int), SetL(l), Ans(LdDF(x, C(0), 1)))
  | Closure.String (d) ->
    let l =
	try
	  (* すでに定数テーブルにあったら再利用 *)
	  let (l, _) = List.find (fun (_, d') -> d = d') !sdata in
	  l
	with Not_found ->
	  let l = Id.L(Id.genid "S") in
	  sdata := (l, d) :: !sdata;
	  l
    in
    let x = Id.genid "S" in
    Let((x, Type.Int), SetL(l), Ans(LdDF(x, C(0), 1)))
  | Closure.Neg(x) -> Ans(Neg(x))
  | Closure.Add(x, y) -> Ans(Add(x, V(y)))
  | Closure.Sub(x, y) -> Ans(Sub(x, V(y)))
  | Closure.FNeg(x) -> Ans(FNegD(x))
  | Closure.FAdd(x, y) -> Ans(FAddD(x, y))
  | Closure.FSub(x, y) -> Ans(FSubD(x, y))
  | Closure.FMul(x, y) -> Ans(FMulD(x, y))
  | Closure.FDiv(x, y) -> Ans(FDivD(x, y))
  | Closure.SConcat (x, y) -> Ans(SConcat(x, y))
  | Closure.IfEq(x, y, e1, e2) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(IfEq(x, V(y), g env e1, g env e2))
      | Type.Float -> Ans(IfFEq(x, y, g env e1, g env e2))
      | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfLE(x, y, e1, e2) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(IfLE(x, V(y), g env e1, g env e2))
      | Type.Float -> Ans(IfFLE(x, y, g env e1, g env e2))
      | _ -> failwith "inequality supported only for bool, int, and float")
  | Closure.Let((x, t1), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (M.add x t1 env) e2 in
      concat e1' (x, t1) e2'
  | Closure.Var(x) ->
      (match M.find x env with
      | Type.Unit -> Ans(Nop)
      | Type.Float -> Ans(FMovD(x))
      | _ -> Ans(Mov(x)))
  | Closure.Mod_fun(f) ->
    let x = "fun " ^ Fun.erl_sig f ^ "/" ^ (string_of_int (List.length f.Type.fun_args)) in
    Ans (Mov x)
  | Closure.MakeCls((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2) -> (* クロージャの生成 (caml2html: virtual_makecls) *)
      (* Closureのアドレスをセットしてから、自由変数の値をストア *)
      let e2' = g (M.add x t env) e2 in
      let offset, store_fv =
	expand
	  (List.map (fun y -> 
                if M.mem y env then (y, M.find y env)
                else (y, Fun.to_typ (Context.find_top_typ y))) ys)
	  (4, e2')
	  (fun y offset store_fv -> seq(StDF(y, x, C(offset), 1), store_fv))
	  (fun y _ offset store_fv -> seq(St(y, x, C(offset), 1), store_fv)) in
      Let((x, t), Mov(reg_hp),
	  Let((reg_hp, Type.Int), Add(reg_hp, C(align offset)),
	      let z = Id.genid "L" in
	      Let((z, Type.Int), SetL(l),
		  seq(St(z, x, C(0), 1),
		      store_fv))))
  | Closure.AppCls(x, ys) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallCls(x, int, float))
  | Closure.AppDir(Id.L(x), ys) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallDir(Id.L(x), int, float))
  | Closure.AppDir(Id.M(x), ys) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallDir(Id.M(x), int, float))
  | Closure.Tuple(xs) -> (* 組の生成 (caml2html: virtual_tuple) *)
    let y = Id.genid "P" in
    Let((y, Type.Tuple(List.map (fun x -> M.find x env) xs)), Tuple xs, Ans (Mov y))
  | Closure.List [] ->
    (* adhoc *)
    let y = Id.genid "C" in
    Let((y, Type.List Unit), List [], Ans (Mov y))
  | Closure.List xs ->
    let y = Id.genid "C" in
    Let((y, Type.List (M.find (List.hd xs) env)), List xs, Ans (Mov y))
  | Closure.LetTuple(xts, y, e2) ->
      let s = Closure.fv e2 in
      let (offset, load) =
	expand
	  xts
	  (0, g (M.add_list xts env) e2)
	  (fun x offset load ->
	    if not (S.mem x s) then load else (* [XX] a little ad hoc optimization *)
	    fletd(x, LdDF(y, C(offset), 1), load))
	  (fun x t offset load ->
	    if not (S.mem x s) then load else (* [XX] a little ad hoc optimization *)
	    Let((x, t), Ld(y, C(offset), 1), load)) in
      load
  | Closure.Get(x, y) -> (* 配列の読み出し (caml2html: virtual_get) *)
      (match M.find x env with
      | Type.Array(Type.Unit) -> Ans(Nop)
      | Type.Array(Type.Float) -> Ans(LdDF(x, V(y), 8))
      | Type.Array(_) -> Ans(Ld(x, V(y), 4))
      | _ -> assert false)
  | Closure.Put(x, y, z) ->
      (match M.find x env with
      | Type.Array(Type.Unit) -> Ans(Nop)
      | Type.Array(Type.Float) -> Ans(StDF(z, x, V(y), 8))
      | Type.Array(_) -> Ans(St(z, x, V(y), 4))
      | _ -> assert false)
  | Closure.ExtArray(Id.L(x)) -> Ans(SetL(Id.L("min_caml_" ^ x)))
  | Closure.ExtArray(Id.M(x)) -> Ans(SetL(Id.L("min_caml_" ^ x))) (* adhoc *)

(* 関数の仮想マシンコード生成 (caml2html: virtual_h) *)
let h { Closure.name = (id, t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e } =
  let x = match id with
    | Id.L (x) -> x
    | Id.M (x) -> x
  in
  let (int, float) = separate (FunArg.vars yts) in
  let (offset, load) =
    expand
      zts
      (4, g (M.add x t (M.add_list (FunArg.vars yts) (M.add_list zts M.empty))) e)
      (fun z offset load -> Asm.fletd(z, LdDF(Asm.reg_cl, C(offset), 1), load))
      (fun z t offset load -> Let((z, t), Ld(Asm.reg_cl, C(offset), 1), load)) in
  match t with
  | Type.Fun { fun_ret = t2 } ->
      { Asm.name = Id.L(x); ptn = yts; args = int; fargs = float; body = load; ret = t2 }
  | _ -> assert false

(* プログラム全体の仮想マシンコード生成 (caml2html: virtual_f) *)
let f (Closure.Prog(fundefs, e)) =
  data := [];
  sdata := [];
  let fundefs = List.map h fundefs in
  let e = g M.empty e in
  Asm.Prog(!data, !sdata, fundefs, e)
