(* give names to intermediate values (K-normalization) *)
(* 変換後のコードをなるべくオリジナルに近いものにするため、実際にはほとんどK正規形にはしない。 *)

open Locating

type t = (* K正規化後の式 (caml2html: knormal_t) *)
    term * Type.t
and term =
  | Unit
  | Exp of et
  | If of et * t * t 
  | Match of Id.t * (pattern * t) list
  | Let of (Id.t * Type.t) * t * t
  | LetRec of fundef * t
and et = 
    expr * Type.t
and expr =
  | Bool of bool
  | Int of int
  | String of string
  | Atom of string
  | Bitstring of Bitstring.t
  | Record of (Id.t * et) list
  | Field of et * Id.t
  | Module of Id.t
  | Tuple of et list
  | Not of et
  | And of et * et
  | Or of et * et
  | Neg of et
  | Add of et * et
  | Sub of et * et
  | Mul of et * et
  | Div of et * et
  | Eq of et * et
  | LE of et * et
  | Var of Id.t
  | Concat of et * et
  | Constr of Id.t * et list
  | App of et * et list
  | ExtFunApp of Id.t * et list
and pattern =
  | PtUnit
  | PtBool of bool
  | PtInt of int
  | PtVar of Id.t * Type.t
  | PtTuple of pattern list
  | PtField of (Id.t * pattern) list
  | PtConstr of Id.t * pattern list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
and def =
  | TypeDef of (Id.t * Type.tycon)
  | VarDef of (Id.t * Type.t) * t
  | RecDef of fundef

let rec ocaml_of_pattern =
  function
  | PtUnit -> "()"
  | PtBool(b) -> string_of_bool b
  | PtInt(n) -> string_of_int n
  | PtVar(x, t) -> x
  | PtTuple(ps) -> String.concat ", " (List.map ocaml_of_pattern ps)
  | PtField(xps) -> String.concat ", " (List.map (fun (x, p) -> x ^ " = " ^ (ocaml_of_pattern p)) xps)
  | PtConstr(x, ps) -> x ^ ", " ^ String.concat ", " (List.map ocaml_of_pattern ps)

let rec string_of_typed_expr (e, t) = (string_of_expr e) ^ " : " ^ (Type.string_of_t t)

and string_of_expr = 
  function
  | Bool(b) -> string_of_bool b
  | Int(n) -> string_of_int n
  | String s -> "\"" ^ s ^ "\""
  | Atom s -> "@\"" ^ s ^ "\""
  | Bitstring x -> Bitstring.to_string x
  | Record(xes) -> "{" ^ (String.concat "; " (List.map (fun (x, e) -> x ^ " = " ^ (string_of_typed_expr e)) xes)) ^ "}"
  | Field(e, x) -> (string_of_typed_expr e) ^ "." ^ x
  | Module x -> "module type " ^ x
  | Tuple(es) -> "(" ^ (String.concat ", " (List.map string_of_typed_expr es)) ^ ")"
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
  let () = Log.debug "KNormal.pattern %s\n" (Syntax.string_of_pattern p) in
  match p.desc with
  | Syntax.PtUnit -> env, PtUnit
  | Syntax.PtBool(b) -> env, (PtBool(b))
  | Syntax.PtInt(n) -> env, (PtInt(n))
  | Syntax.PtVar(x, t) -> Env.add_var env x t, (PtVar(x, t))
  | Syntax.PtTuple(ps) -> 
      let env, ps' = List.fold_left (fun (env, ps) p -> let env', p' = pattern env p in env', p' :: ps) (env, []) (List.rev ps) in
      env, PtTuple(ps')
  | Syntax.PtRecord(xps) -> 
      let env, xps' = List.fold_left (fun (env, xps) (x, p) -> let env', p' = pattern env p in env', (x, p') :: xps) (env, []) (List.rev xps) in
      env, PtField(xps')
  | Syntax.PtConstr(x, ps) -> 
      let env, ps' = List.fold_left (fun (env, ps) p -> let env', p' = pattern env p in env', p' :: ps) (env, []) (List.rev ps) in
      env, PtConstr(x, ps')
        
let rec g ({ Env.venv = venv; tenv = tenv } as env) { desc = (e, t) } = (* K正規化ルーチン本体 (caml2html: knormal_g) *)
  let _ = Log.debug "kNormal.g %s\n" (Syntax.string_of_expr e) in  

  let insert_lets es k =
    let rec insert_lets' es k args =
      match es with
      | [] -> k args
      | (e::es') -> insert_let (g env e) (fun et' -> insert_lets' es' k (args @ [et'])) in
    insert_lets' es k [] in

  let binop e1 e2 f =
    insert_let (g env e1)
      (fun e1' -> insert_let (g env e2)
        (fun e2' -> f e1' e2')) in

  let e' = 
    match e with
    | Syntax.Unit -> Unit
    | Syntax.Bool(b) -> Exp(Bool(b), t)
    | Syntax.Int(n) -> Exp(Int(n), t)
    | Syntax.String(s) -> Exp(String(s), t)
    | Syntax.Atom(s) -> Exp(Atom(s), t)
    | Syntax.Bitstring(x) -> Exp(Bitstring(x), t)
    | Syntax.Record(xes) ->
      insert_lets (List.map snd xes)
        (fun ets' -> Exp(Record(List.combine (List.map fst xes) ets'), t))
    | Syntax.Field(e, x) -> insert_let (g env e) (fun e' -> Exp(Field(e', x), t))
    | Syntax.Module x -> Exp(Module x, t)
    | Syntax.Tuple(es) -> insert_lets es (fun es' -> Exp(Tuple(es'), t))
    | Syntax.Not(e) -> insert_let (g env e) (fun e' -> Exp(Not(e'), t))
    | Syntax.And(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(And(e1', e2'), t))
    | Syntax.Or(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Or(e1', e2'), t))
    | Syntax.Neg(e) -> insert_let (g env e) (fun e' -> Exp(Neg(e'), t))
    | Syntax.Add(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Add(e1', e2'), t)) (* 足し算のK正規化 (caml2html: knormal_add) *)
    | Syntax.Sub(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Sub(e1', e2'), t))
    | Syntax.Mul(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Mul(e1', e2'), t))
    | Syntax.Div(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Div(e1', e2'), t))
    | Syntax.Var(x) -> Exp(Var(x), t)
    | Syntax.Concat(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Concat(e1', e2'), t))
    | Syntax.Constr(x, es) -> insert_lets es (fun es' -> Exp(Constr(x, es'), t))
    | Syntax.Eq(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(Eq(e1', e2'), t))
    | Syntax.LE(e1, e2) -> binop e1 e2 (fun e1' e2' -> Exp(LE(e1', e2'), t))
    | Syntax.If(e1, e2, e3) -> insert_let (g env e1) (fun e1' -> If(e1', (g env e2), (g env e3)))
    | Syntax.Match({ desc = (Syntax.Var(x), _) }, pes) ->
        let pes' = List.map 
          (fun (p, e) -> 
            let env', p' = pattern env p in
            let e' = g env' e in 
            p', e')
          pes in
        Match(x, pes')
    | Syntax.Match(e, pes) ->
        let e' = g env e in
        let pes' = List.map 
          (fun (p, e) -> 
            let env', p' = pattern env p in
            let e' = g env' e in 
            p', e')
          pes in
        let x = Id.gentmp (Type.prefix t) in
        Let((x, t), e', (Match(x, pes'), t))
    | Syntax.LetVar((x, t), e1, e2) ->
        let e1' = g env e1 in
        let e2' = g (Env.add_var env x t) e2 in
        Let((x, t), e1', e2')
    | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
        let venv' = M.add x t venv in
        let e2' = g { env with Env.venv = venv' } e2 in
        let e1' = g { env with Env.venv = M.add_list yts venv' } e1 in
        LetRec({ name = (x, t); args = yts; body = e1' }, e2')
    | Syntax.App({ desc = (Syntax.Var(f), _) }, e2s) when not (M.mem f venv) -> (* 外部関数の呼び出し (caml2html: knormal_extfunapp) *)
      Log.debug "# external variable `%s'\n" f;
      assert false
    | Syntax.App({ desc = (Syntax.Var(f), _) }, e2s)
      when Env.is_module_val env f ->
      let m = Env.find_module_of_val env f in
      Log.debug "# applying %s.%s (full imported)\n" m.Module.name f;
      let f' = Module.primitive m f in
      let rec bind xs = (* "xs" are identifiers for the arguments *)
        function
        | [] -> Exp(ExtFunApp(f', xs), t)
        | e2 :: e2s -> insert_let (g env e2) (fun x -> bind (xs @ [x]) e2s) in
      (bind [] e2s) (* left-to-right evaluation *)
    | Syntax.App(e1, e2s) ->
        insert_let (g env e1)
          (fun f ->
            let rec bind xs = (* "xs" are identifiers for the arguments *)
              function 
              | [] -> Exp(App(f, xs), t)
              | e2 :: e2s -> insert_let (g env e2) (fun x -> bind (xs @ [x]) e2s) in
            bind [] e2s) (* left-to-right evaluation *)
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
  Syntax.fold (fun (env, defs) def ->
    match def.desc with
    | Syntax.TypeDef(x, t) -> TypeDef(x, t) :: defs
    | Syntax.VarDef((x, t), e) -> VarDef((x, t), f' env e) :: defs
    | Syntax.RecDef({ Syntax.name = (x, t); args = yts; body = e }) ->
        RecDef({ name = (x, t); args = yts; body = f' env e }) :: defs
    | _ -> assert false) defs (Sig.create_env ())
