type closure = { entry : Id.l; actual_fv : Id.t list }
type t = (* ���������Ѵ���μ� (caml2html: closure_t) *)
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
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | Mod_fun of Fun.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | List of Id.t list
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.l
type fundef = {
  name : Id.l * Type.t;
  args : FunArg.t list;
  formal_fv : (Id.t * Type.t) list;
  body : t }
type prog = Prog of fundef list * t

let rec fv = function
  | Unit | Bool _ | Int(_) | Float(_) | String _ | ExtArray(_) | Mod_fun _ -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | SConcat (x, y) | Get(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2)| IfLE(x, y, e1, e2) ->
    S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) ->
    S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls(x, ys) -> S.of_list (x :: ys)
  | AppDir(_, xs) | Tuple(xs) | List xs -> S.of_list xs
  | LetTuple(xts, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xts)))
  | Put(x, y, z) -> S.of_list [x; y; z]

let toplevel : fundef list ref = ref []

let rec g env known = function (* ���������Ѵ��롼�������� (caml2html: closure_g) *)
  | KNormal.Unit -> Unit
  | KNormal.Bool b -> Bool b
  | KNormal.Int(i) -> Int(i)
  | KNormal.Float(d) -> Float(d)
  | KNormal.String s -> String s
  | KNormal.Neg(x) -> Neg(x)
  | KNormal.Add(x, y) -> Add(x, y)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.FNeg(x) -> FNeg(x)
  | KNormal.FAdd(x, y) -> FAdd(x, y)
  | KNormal.FSub(x, y) -> FSub(x, y)
  | KNormal.FMul(x, y) -> FMul(x, y)
  | KNormal.FDiv(x, y) -> FDiv(x, y)
  | KNormal.SConcat (x, y) -> SConcat (x, y)
  | KNormal.IfEq(x, y, e1, e2) -> IfEq(x, y, g env known e1, g env known e2)
  | KNormal.IfLE(x, y, e1, e2) -> IfLE(x, y, g env known e1, g env known e2)
  | KNormal.Let((x, t), e1, e2) -> Let((x, t), g env known e1, g (M.add x t env) known e2)
  | KNormal.Var(x) -> Var(x)
  | KNormal.Mod_fun f -> Mod_fun f
  | KNormal.Def { KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 } -> (* �ؿ�����ξ�� (caml2html: closure_letrec) *)
      (* �ؿ����let rec x y1 ... yn = e1 in e2�ξ��ϡ�
	 x�˼�ͳ�ѿ����ʤ�(closure��𤵤�direct�˸ƤӽФ���)
	 �Ȳ��ꤷ��known���ɲä���e1�򥯥������Ѵ����Ƥߤ� *)
    let toplevel_backup = !toplevel in
    let env' = M.add x t env in
    let known' = S.add x known in
    let e1' = g (M.add_list (FunArg.vars yts) env') known' e1 in
    (* �����˼�ͳ�ѿ����ʤ��ä������Ѵ����e1'���ǧ���� *)
    (* ���: e1'��x���Ȥ��ѿ��Ȥ��ƽи��������closure��ɬ��!
    (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml����) *)
    let zs = S.diff (fv e1') (S.of_list (FunArg.names yts)) in
    let known', e1' =
      if S.is_empty zs then
        known', e1'
      else begin
        (* ���ܤ��ä������(toplevel����)���ᤷ�ơ����������Ѵ�����ľ�� *)
        Log.debug "# free variable(s) %s found in function `%s'\n" (Id.pp_list (S.elements zs)) x;
        Log.debug "# function `%s' cannot be directly applied in fact\n" x;
        toplevel := toplevel_backup;
        let e1' = g (M.add_list (FunArg.vars yts) env') known e1 in
        known, e1'
      end
     in
     let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (FunArg.names yts)))) in (* ��ͳ�ѿ��Υꥹ�� *)
     let zts = List.map
                 (* �����Ǽ�ͳ�ѿ�z�η����������˰���env��ɬ�� *)
                 (fun z -> if M.mem z env' then (z, M.find z env')
                  else (z, Fun.to_typ (Context.find_top_typ z))) zs in 
     toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel; (* �ȥåץ�٥�ؿ����ɲ� *)

     MakeCls ((x, t), { entry = Id.L(x); actual_fv = zs }, Unit)
      (*
      let e2' = g env' known' e2 in
      if S.mem x (fv e2') then (* x���ѿ��Ȥ���e2'�˽и����뤫 *)
    MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2') (* �и����Ƥ����������ʤ� *)
      else
    (Format.eprintf "eliminating closure(s) %s@." x;
     e2') (* �и����ʤ����MakeCls���� *)
       *)
  | KNormal.App(x, ys) when S.mem x known -> (* �ؿ�Ŭ�Ѥξ�� (caml2html: closure_app) *)
    Log.debug "# directly applying %s\n" x;
    AppDir(Id.L(x), ys)
  | KNormal.App(x, xs) ->
    if M.mem x env then
      (* external function *)
      match M.find x env with
      | Type.Fun ({ fun_mod = Some _ } as f) -> AppDir(Id.M (Fun.erl_sig f), xs)
      | e -> AppCls(x, xs)
    else begin
      Log.debug "# closure applying %s\n" x;
      AppCls(x, xs)
    end
  | KNormal.Tuple(xs) -> Tuple(xs)
  | KNormal.List xs -> List xs
  | KNormal.LetTuple(xts, y, e) -> LetTuple(xts, y, g (M.add_list xts env) known e)
  | KNormal.Get(x, y) -> Get(x, y)
  | KNormal.Put(x, y, z) -> Put(x, y, z)
  | KNormal.ExtArray(x) -> ExtArray(Id.L(x))
  | KNormal.ExtFunApp(x, ys) -> AppDir(Id.L("bran_pervasives:" ^ x), ys)

let f e =
  toplevel := [];
  let e' = g (Env.create ()) S.empty e in
  Prog(List.rev !toplevel, e')
