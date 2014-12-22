(* give names to intermediate values (K-normalization) *)
(* 変換後のコードをなるべくオリジナルに近いものにするため、実際にはほとんどK正規形にはしない。 *)

open KNormal_t
open With.Loc
open Base

let rec ocaml_of_pattern =
  function
  | PtUnit -> "()"
  | PtBool(b) -> string_of_bool b
  | PtInt(n) -> IntRepr.to_string n
  | PtFloat v -> string_of_float v
  | PtAtom(v) -> "@\"" ^ v ^ "\""
  | PtString(v) -> "\"" ^ v ^ "\""
  | PtVar(x, t) -> x
  | PtAlias (p, x, _) -> ocaml_of_pattern p ^ " as " ^ x
  | PtTuple(ps) -> String.concat_map ", " ocaml_of_pattern ps
  | PtList(ps) -> String.concat_map ", " ocaml_of_pattern ps
  | PtCons (p1, p2) -> (ocaml_of_pattern p1) ^ "::" ^ (ocaml_of_pattern p2)
  | PtField(xps) -> String.concat ", " (List.map (fun (x, p) -> x ^ " = " ^ (ocaml_of_pattern p)) xps)
  | PtConstr(x, ps) ->
    (Binding.to_string x) ^ ", " ^ String.concat_map ", " ocaml_of_pattern ps

let rec string_of_typed_expr (e, t) = (string_of_expr e) ^ " : " ^ (Type.to_string t)

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
  | List(es) -> "[" ^ (String.concat_map ", " string_of_typed_expr es) ^ "]"
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
  | Var(`Local x) -> "Var(`Local " ^ x ^ ")"
  | Var(`Module x) -> "Var(`Module " ^ (Binding.to_string x) ^ ")"
  | Concat(e1, e2) -> (string_of_typed_expr e1) ^ " ^ " ^ (string_of_typed_expr e2)
  | Constr(x, es) -> "Constr(" ^ (Binding.to_string x) ^ ", [" ^ (String.concat ", " (List.map string_of_typed_expr es)) ^ "])"
  | Eq(e1, e2) -> (string_of_typed_expr e1) ^ " = " ^ (string_of_typed_expr e2)
  | LE(e1, e2) -> (string_of_typed_expr e1) ^ " <= " ^ (string_of_typed_expr e2) 
  | App(e, args) -> "App(" ^ (string_of_typed_expr e) ^ ", [" ^ (String.concat ", " (List.map string_of_typed_expr args)) ^ "])"
  | ExtFunApp(x, args) -> "ExtFunApp(" ^ x ^ ", [" ^ (String.concat " " (List.map string_of_typed_expr args)) ^ "])"
  | Get (e1, e2) ->
    Printf.sprintf "Get(%s, %s)" (string_of_typed_expr e1) (string_of_typed_expr e2)
  | Put (e1, e2, e3) ->
    Printf.sprintf "Put(%s, %s, %s)"
      (string_of_typed_expr e1) (string_of_typed_expr e2) (string_of_typed_expr e3)

let rec string_of_typed_term (e, t) = (string_of_term e) ^ " : " ^ (Type.to_string t)

and string_of_term = 
  function
  | Unit -> "()"
  | Exp(e) -> "Exp(" ^ string_of_typed_expr e ^ ")"
  | If(e, e1, e2) -> "If(" ^ (string_of_typed_expr e) ^ "then " ^ (string_of_typed_term e1) ^ "else " ^ (string_of_typed_term e2) ^ ")"
  | Match(x, pes) -> "Match(" ^ x ^ ", [" ^ (String.concat "" (List.map (fun (p, e) -> " | " ^ (ocaml_of_pattern p) ^ " -> " ^ (string_of_typed_term e)) pes)) ^ "])"
  | Let((s1, t), e1, e2) -> "Let(" ^ s1 ^ " : " ^ (Type.to_string  t) ^ " = " ^ (string_of_typed_term e1) ^ " in " ^ (string_of_typed_term e2) ^ ")"
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> 
      "LetRec(" ^ x ^ ", [" ^ (String.concat ", " (List.map (fun (y, t) -> y) yts)) ^ " : " ^ (Type.to_string  t) ^ "] = "
      ^ (string_of_typed_term e1) ^ " in " ^ (string_of_typed_term e2) ^ ")"

let rec insert_let (e, t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e with
  | Exp(e) -> k e
  | LetRec(fundef, et) ->
      let e' = insert_let et k in
      LetRec(fundef, (e', t))
  | _ ->
      let x = Id.gentmp (Type.prefix t) in
      let e' = k (Var(`Local x), t) in
      Let((x, t), (e, t), (e', t))

let rec pattern env p = 
  Log.debug "KNormal.pattern %s\n" (Ast.Pattern.to_string p);
  let open Ast.Pattern in
  match p.desc with
  | Ast_t.PtUnit -> env, PtUnit
  | Ast_t.PtBool(b) -> env, PtBool b
  | Ast_t.PtInt(n) -> env, PtInt n
  | Ast_t.PtFloat v -> env, PtFloat v
  | Ast_t.PtAtom v -> env, PtAtom v
  | Ast_t.PtString v -> env, PtString v
  | Ast_t.PtVar(x, t) -> Env.add_var env x t, (PtVar(x, t))
  | Ast_t.PtAlias (p, x, t) ->
    let env', p' = pattern env p in
    Env.add_var env x t, (PtAlias (p', x, t))
  | Ast_t.PtTuple(ps) -> 
    fold (fun ps' -> PtTuple ps') pattern env ps
  | Ast_t.PtList(ps) -> 
    fold (fun ps' -> PtList ps') pattern env ps
  | Ast_t.PtCons (p1, p2) ->
    fold_bin (fun p1' p2' -> PtCons (p1', p2')) pattern env p1 p2
  | Ast_t.PtRecord(xps) -> 
    fold_assoc (fun xps' -> PtField xps') pattern env xps
  | Ast_t.PtConstr(x, ps, _) -> 
    fold (fun ps' -> PtConstr (x, ps')) pattern env ps

let rec g ({ Env.venv = venv; tenv = tenv } as env) { tag = loc; desc = (e, t) } = (* K正規化ルーチン本体 (caml2html: knormal_g) *)
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
    | Ast_t.List(es) -> insert_lets es (fun es' -> Exp(List(es'), t))
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
    | Ast_t.Var(`Local x) -> Exp(Var(`Local x), t)
    | Ast_t.Var(`Module x) -> Exp(Var(`Module x), t)
    | Ast_t.Var(`Unbound _) -> assert false
    | Ast_t.Concat(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Concat(e1', e2'), t))
    | Ast_t.Constr(x, es) -> insert_lets es (fun es' -> Exp(Constr(x, es'), t))
    | Ast_t.Eq(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Eq(e1', e2'), t))
    | Ast_t.LE(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(LE(e1', e2'), t))
    | Ast_t.If(e1, e2, e3) -> insert_let (g env e1) (fun e1' -> If(e1', (g env e2), (g env e3)))
    | Ast_t.Match({ desc = (Ast_t.Var(`Local x), _) }, pes) ->
      let pes' = List.map 
          (fun (p, e) -> 
             let env', p' = pattern env p in
             let e' = g env' e in 
             p', e')
          pes
      in
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
          (*
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
           *)
    | Ast_t.App(e1, e2s) ->
        insert_let (g env e1)
          (fun f ->
            let rec bind xs = (* "xs" are identifiers for the arguments *)
              function 
              | [] -> Exp(App(f, xs), t)
              | e2 :: e2s -> insert_let (g env e2) (fun x -> bind (xs @ [x]) e2s) in
            bind [] e2s) (* left-to-right evaluation *)
    | Ast_t.Get (e1, e2) ->
      let (_, t) as g_e1 = g env e1 in
      begin match t.desc with
        | Type_t.App(Type_t.Array, [t]) ->
          insert_let g_e1 (fun x -> insert_let (g env e2)
                              (fun y -> Exp (Get(x, y), t)))
        | _ -> assert false
      end
    | Ast_t.Put (e1, e2, e3) ->
      triple_of_insert_lets e1 e2 e3
        (fun x y z -> Exp (Put (x, y, z), Type.app_unit loc))
    | Ast_t.Perform _ -> failwith "not implemented"
    | Ast_t.Bind _ -> failwith "not implemented"
    | Ast_t.Return _ -> failwith "not implemented"
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
            Env.venv = M.add_list (Type.Tycon.vars t) venv;
            Env.tenv = M.add_list (Type.Tycon.types t) tenv } in
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
