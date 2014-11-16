(* rename identifiers to make them unique (alpha-conversion) *)

let find x env = try M.find x env with Not_found -> x

let rec g env e = (* α変換ルーチン本体 (caml2html: alpha_g) *)
  let open KNormal in
  match e with
  | Unit -> Unit
  | Bool b -> Bool b
  | Int(i) -> Int(i)
  | Float(d) -> Float(d)
  | String s -> String s
  | Neg(x) -> Neg(find x env)
  | Add(x, y) -> Add(find x env, find y env)
  | Sub(x, y) -> Sub(find x env, find y env)
  | FNeg(x) -> FNeg(find x env)
  | FAdd(x, y) -> FAdd(find x env, find y env)
  | FSub(x, y) -> FSub(find x env, find y env)
  | FMul(x, y) -> FMul(find x env, find y env)
  | FDiv(x, y) -> FDiv(find x env, find y env)
  | SConcat (x, y) -> SConcat (find x env, find y env)
  | IfEq(x, y, e1, e2) -> IfEq(find x env, find y env, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(find x env, find y env, g env e1, g env e2)
  | Let((x, t), e1, e2) -> (* letのα変換 (caml2html: alpha_let) *)
      let x' = Id.genid x in
      Let((x', t), g env e1, g (M.add x x' env) e2)
  | Var(x) -> Var(find x env)
  | Mod_fun(f) -> Mod_fun(f)
  | Def { name = (x, t); rec_ = rec_; args = yts; body = e1 } ->
    let x' = Id.genid x in
    Context.add_top_fun x x';
    let env = M.add x x' env in
    let ys = FunArg.names yts in
    let env' = M.add_list2 ys (List.map Id.genid ys) env in
    Def { name = (find x env, t);
          rec_ = rec_;
	      args = g_funargs env' yts;
	      body = g env' e1 }
  | App(x, ys) -> App(find x env, List.map (fun y -> find y env) ys)
  | Tuple(xs) -> Tuple(List.map (fun x -> find x env) xs)
  | List(xs) -> List(List.map (fun x -> find x env) xs)
  | LetTuple(xts, y, e) -> (* LetTupleのα変換 (caml2html: alpha_lettuple) *)
      let xs = List.map fst xts in
      let env' = M.add_list2 xs (List.map Id.genid xs) env in
      LetTuple(List.map (fun (x, t) -> (find x env', t)) xts,
	       find y env,
	       g env' e)
  | Get(x, y) -> Get(find x env, find y env)
  | Put(x, y, z) -> Put(find x env, find y env, find z env)
  | ExtArray(x) -> ExtArray(x)
  | ExtFunApp(x, ys) -> ExtFunApp(x, List.map (fun y -> find y env) ys)

and g_funargs env args = 
  let open FunArg in
  let g' = function
    | Var (y, t) -> Var (find y env, t)
    | Tuple (y, yts) -> Tuple (find y env, g_funargs env yts)
  in
  List.map g' args

let f = g M.empty
