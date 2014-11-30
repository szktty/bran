(* give names to intermediate values (K-normalization) *)
(* 変換後のコードをなるべくオリジナルに近いものにするため、実際にはほとんどK正規形にはしない。 *)

open KNormal_t
open Locating
open X

let rec ocaml_of_pattern =
  function
  | PtUnit -> "()"
  | PtBool(b) -> string_of_bool b
  | PtInt(n) -> IntRepr.to_string n
  | PtVar(x, t) -> x
  | PtTuple(ps) -> String.concat ", " (List.map ocaml_of_pattern ps)
  | PtField(xps) -> String.concat ", " (List.map (fun (x, p) -> x ^ " = " ^ (ocaml_of_pattern p)) xps)
  | PtConstr(x, ps) -> x ^ ", " ^ String.concat ", " (List.map ocaml_of_pattern ps)

let rec string_of_typed_expr (e, t) = (string_of_expr e) ^ " : " ^ (Type.string_of_t t)

and string_of_expr = 
  function
  | Bool(b) -> string_of_bool b
  | Int(n) -> IntRepr.to_string n
  | Float f -> string_of_float f
  | Char s -> "'" ^ s ^ "'"
  | String s -> "\"" ^ s ^ "\""
  | Atom s -> "@\"" ^ s ^ "\""
  | Bitstring x -> Bitstring.to_string x
  | Record(xes) -> "{" ^ (String.concat "; " (List.map (fun (x, e) -> x ^ " = " ^ (string_of_typed_expr e)) xes)) ^ "}"
  | Field(e, x) -> (string_of_typed_expr e) ^ "." ^ x
  | Module x -> "module type " ^ x
  | Tuple(es) -> "(" ^ (String.concat_map ", " string_of_typed_expr es) ^ ")"
  | Array(es) -> "[|" ^ (String.concat_map "; " string_of_typed_expr es) ^ "|]"
  | Not(e) -> "not " ^ (string_of_typed_expr e)
  | And(e1, e2) -> (string_of_typed_expr e1) ^ " && " ^ (string_of_typed_expr e2)
  | Or(e1, e2) -> (string_of_typed_expr e1) ^ " || " ^ (string_of_typed_expr e2)
  | Neg(e) -> "! " ^ (string_of_typed_expr e)
  | Add(e1, e2) -> (string_of_typed_expr e1) ^ " + " ^ (string_of_typed_expr e2)
  | Sub(e1, e2) -> (string_of_typed_expr e1) ^ " - " ^ (string_of_typed_expr e2)
  | Mul(e1, e2) -> (string_of_typed_expr e1) ^ " * " ^ (string_of_typed_expr e2)
  | Div(e1, e2) -> (string_of_typed_expr e1) ^ " / " ^ (string_of_typed_expr e2)
  | Var(x) -> "Var(" ^ x ^ ")"
  | Concat(e1, e2) -> (string_of_typed_expr e1) ^ " ^ " ^ (string_of_typed_expr e2)
  | Constr(x, es) -> "Constr(" ^ x ^ ", [" ^ (String.concat ", " (List.map string_of_typed_expr es)) ^ "])"
  | Eq(e1, e2) -> (string_of_typed_expr e1) ^ " = " ^ (string_of_typed_expr e2)
  | LE(e1, e2) -> (string_of_typed_expr e1) ^ " <= " ^ (string_of_typed_expr e2) 
  | App(e, args) -> "App(" ^ (string_of_typed_expr e) ^ ", [" ^ (String.concat ", " (List.map string_of_typed_expr args)) ^ "])"
  | ExtFunApp(x, args) -> "ExtFunApp(" ^ x ^ ", [" ^ (String.concat " " (List.map string_of_typed_expr args)) ^ "])"
  | Get (e1, e2) ->
    Printf.sprintf "Get(%s, %s)" (string_of_typed_expr e1) (string_of_typed_expr e2)
  | Put (e1, e2, e3) ->
    Printf.sprintf "Put(%s, %s, %s)"
      (string_of_typed_expr e1) (string_of_typed_expr e2) (string_of_typed_expr e3)

let rec string_of_typed_term (e, t) = (string_of_term e) ^ " : " ^ (Type.string_of_t t)

and string_of_term = 
  function
  | Unit -> "()"
  | Exp(e) -> "Exp(" ^ string_of_typed_expr e ^ ")"
  | If(e, e1, e2) -> "If(" ^ (string_of_typed_expr e) ^ "then " ^ (string_of_typed_term e1) ^ "else " ^ (string_of_typed_term e2) ^ ")"
  | Match(x, pes) -> "Match(" ^ x ^ ", [" ^ (String.concat "" (List.map (fun (p, e) -> " | " ^ (ocaml_of_pattern p) ^ " -> " ^ (string_of_typed_term e)) pes)) ^ "])"
  | Let((s1, t), e1, e2) -> "Let(" ^ s1 ^ " : " ^ (Type.string_of_t  t) ^ " = " ^ (string_of_typed_term e1) ^ " in " ^ (string_of_typed_term e2) ^ ")"
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> 
      "LetRec(" ^ x ^ ", [" ^ (String.concat ", " (List.map (fun (y, t) -> y) yts)) ^ " : " ^ (Type.string_of_t  t) ^ "] = "
      ^ (string_of_typed_term e1) ^ " in " ^ (string_of_typed_term e2) ^ ")"

let rec insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e with
  | Exp(e) -> k e
  | LetRec(fundef, et) ->
      let e' = insert_let et k in
      LetRec(fundef, (e', t))
  | _ ->
      let x = Id.gentmp (Type.prefix t) in
      let e' = k (Var(x), t) in
      Let((x, t), (e, t), (e', t))

let rec pattern env p = 
  Log.debug "KNormal.pattern %s\n" (Ast.string_of_pattern p);
  match p.desc with
  | Ast_t.PtUnit -> env, PtUnit
  | Ast_t.PtBool(b) -> env, (PtBool(b))
  | Ast_t.PtInt(n) -> env, (PtInt(n))
  | Ast_t.PtVar(x, t) -> Env.add_var env x t, (PtVar(x, t))
  | Ast_t.PtTuple(ps) -> 
      let env, ps' = List.fold_left (fun (env, ps) p -> let env', p' = pattern env p in env', p' :: ps) (env, []) (List.rev ps) in
      env, PtTuple(ps')
  | Ast_t.PtRecord(xps) -> 
      let env, xps' = List.fold_left (fun (env, xps) (x, p) -> let env', p' = pattern env p in env', (x, p') :: xps) (env, []) (List.rev xps) in
      env, PtField(xps')
  | Ast_t.PtConstr(x, ps) -> 
      let env, ps' = List.fold_left (fun (env, ps) p -> let env', p' = pattern env p in env', p' :: ps) (env, []) (List.rev ps) in
      env, PtConstr(x, ps')
        
let rec g ({ Env.venv = venv; tenv = tenv } as env) { desc = (e, t) } = (* K正規化ルーチン本体 (caml2html: knormal_g) *)
  Log.debug "kNormal.g %s\n" (Ast.string_of_expr e);
  let insert_lets es k =
    let rec insert_lets' es k args =
      match es with
      | [] -> k args
      | (e::es') -> insert_let (g env e) (fun et' -> insert_lets' es' k (args @ [et'])) in
    insert_lets' es k [] in

  let triple_of_insert_lets e1 e2 e3 k =
    insert_lets [e1; e2; e3]
      (fun es -> k (List.nth es 0) (List.nth es 1) (List.nth es 2))
  in

  let binop e1 e2 f =
    insert_let (g env e1)
      (fun e1' -> insert_let (g env e2)
        (fun e2' -> f e1' e2')) in

  let e' = 
    match e with
    | Ast_t.Unit -> Unit
    | Ast_t.Bool(b) -> Exp(Bool(b), t)
    | Ast_t.Int(n) -> Exp(Int(n), t)
    | Ast_t.Float(n) -> Exp(Float(n), t)
    | Ast_t.Char(n) -> Exp(Char(n), t)
    | Ast_t.String(s) -> Exp(String(s), t)
    | Ast_t.Atom(s) -> Exp(Atom(s), t)
    | Ast_t.Bitstring(x) -> Exp(Bitstring(x), t)
    | Ast_t.Record(xes) ->
      insert_lets (List.map snd xes)
        (fun ets' -> Exp(Record(List.combine (List.map fst xes) ets'), t))
    | Ast_t.Field(e, x) -> insert_let (g env e) (fun e' -> Exp(Field(e', x), t))
    | Ast_t.Module x -> Exp(Module x, t)
    | Ast_t.Tuple(es) -> insert_lets es (fun es' -> Exp(Tuple(es'), t))
    | Ast_t.Array(es) -> insert_lets es (fun es' -> Exp(Array(es'), t))
    | Ast_t.Not(e) -> insert_let (g env e) (fun e' -> Exp(Not(e'), t))
    | Ast_t.And(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(And(e1', e2'), t))
    | Ast_t.Or(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Or(e1', e2'), t))
    | Ast_t.Neg(e) -> insert_let (g env e) (fun e' -> Exp(Neg(e'), t))
    | Ast_t.Add(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Add(e1', e2'), t)) (* 足し算のK正規化 (caml2html: knormal_add) *)
    | Ast_t.Sub(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Sub(e1', e2'), t))
    | Ast_t.Mul(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Mul(e1', e2'), t))
    | Ast_t.Div(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Div(e1', e2'), t))
    | Ast_t.Var(x) -> Exp(Var(x), t)
    | Ast_t.Concat(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Concat(e1', e2'), t))
    | Ast_t.Constr(x, es) -> insert_lets es (fun es' -> Exp(Constr(x, es'), t))
    | Ast_t.Eq(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Eq(e1', e2'), t))
    | Ast_t.LE(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(LE(e1', e2'), t))
    | Ast_t.If(e1, e2, e3) -> insert_let (g env e1) (fun e1' -> If(e1', (g env e2), (g env e3)))
    | Ast_t.Match({ desc = (Ast_t.Var(x), _) }, pes) ->
        let pes' = List.map 
          (fun (p, e) -> 
            let env', p' = pattern env p in
            let e' = g env' e in 
            p', e')
          pes in
        Match(x, pes')
    | Ast_t.Match(e, pes) ->
        let e' = g env e in
        let pes' = List.map 
          (fun (p, e) -> 
            let env', p' = pattern env p in
            let e' = g env' e in 
            p', e')
          pes in
        let x = Id.gentmp (Type.prefix t) in
        Let((x, t), e', (Match(x, pes'), t))
    | Ast_t.LetVar((x, t), e1, e2) ->
        let e1' = g env e1 in
        let e2' = g (Env.add_var env x t) e2 in
        Let((x, t), e1', e2')
    | Ast_t.LetRec({ Ast_t.name = (x, t); Ast_t.args = yts; Ast_t.body = e1 }, e2) ->
        let venv' = M.add x t venv in
        let e2' = g { env with Env.venv = venv' } e2 in
        let e1' = g { env with Env.venv = M.add_list yts venv' } e1 in
        LetRec({ name = (x, t); args = yts; body = e1' }, e2')
    | Ast_t.App({ desc = (Ast_t.Var(f), _) }, e2s) when not (M.mem f venv) -> (* 外部関数の呼び出し (caml2html: knormal_extfunapp) *)
      Log.debug "# external variable `%s'\n" f;
      assert false
    | Ast_t.App({ desc = (Ast_t.Var(f), _) }, e2s)
      when Env.is_module_val env f ->
      let m = Env.find_module_of_val env f in
      Log.debug "# applying %s.%s (full imported)\n" m.Module.name f;
      let f' = Module.primitive m f in
      let rec bind xs = (* "xs" are identifiers for the arguments *)
        function
        | [] -> Exp(ExtFunApp(f', xs), t)
        | e2 :: e2s -> insert_let (g env e2) (fun x -> bind (xs @ [x]) e2s) in
      (bind [] e2s) (* left-to-right evaluation *)
    | Ast_t.App(e1, e2s) ->
        insert_let (g env e1)
          (fun f ->
            let rec bind xs = (* "xs" are identifiers for the arguments *)
              function 
              | [] -> Exp(App(f, xs), t)
              | e2 :: e2s -> insert_let (g env e2) (fun x -> bind (xs @ [x]) e2s) in
            bind [] e2s) (* left-to-right evaluation *)
    | Ast_t.Get (e1, e2) ->
      begin match g env e1 with
        | _, Type_t.App(Type_t.Array, [t]) as g_e1 ->
          insert_let g_e1 (fun x -> insert_let (g env e2)
                              (fun y -> Exp (Get(x, y), t)))
        | _ -> assert false
      end
    | Ast_t.Put (e1, e2, e3) ->
      triple_of_insert_lets e1 e2 e3
        (fun x y z -> Exp (Put (x, y, z), Type.app_unit))
  in
  (e', t)

let fold f env defs = 
  let _, defs' = List.fold_left f (env, []) defs in
  List.rev defs'

let map f defs =
  let f' (({ Env.venv = venv; tenv = tenv } as env), defs) def =
                            Log.debug "# KNormal.map import mv %d\n" (List.length env.Env.mods);
    let env', def' = 
      match def with 
      | TypeDef(x, t) -> 
          let env' = { env with 
            Env.venv = M.add_list (Type.vars t) venv;
            Env.tenv = M.add_list (Type.types t) tenv } in
          env', f env' def
      | VarDef((x, t), e) ->  
          Env.add_var env x t, f env def
      | RecDef({ name = (x, t); args = yts; body = e1 }) -> 
          let env' = Env.add_var env x t in
          env', f env' def in
    env', (def' :: defs) in
  fold f' (Sig.create_env ()) defs

let f' env e = g env e

let f defs =
  Ast.fold (fun (env, defs) def ->
    match def.desc with
    | Ast_t.TypeDef(x, t) -> TypeDef(x, t) :: defs
    | Ast_t.VarDef((x, t), e) -> VarDef((x, t), f' env e) :: defs
    | Ast_t.RecDef({ Ast_t.name = (x, t); args = yts; body = e }) ->
        RecDef({ name = (x, t); args = yts; body = f' env e }) :: defs
    | _ -> assert false) defs (Sig.create_env ())
