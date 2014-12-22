open Ast_t
open Base

let rec string_of_pattern { With.Loc.desc = p } =
  match p with
  | PtUnit -> "PtUnit"
  | PtBool(b) -> "PtBool(" ^ (string_of_bool b) ^ ")"
  | PtInt (b, v) -> Printf.sprintf "PtInt(%d, %s)" b v
  | PtFloat v -> Printf.sprintf "PtFloat(%f)" v
  | PtAtom(v) -> "PtAtom(" ^ v ^ ")"
  | PtString(v) -> "PtString(" ^ v ^ ")"
  | PtVar(x, t) -> "PtVar(" ^ x ^ "," ^ (Type.to_string t) ^ ")"
  | PtAlias (p, x, t) ->
    Printf.sprintf "PtAlias(%s, %s, %s)" (string_of_pattern p) x (Type.to_string t)
  | PtTuple(ps) -> "PtTuple(" ^ (String.concat_map "; " string_of_pattern ps) ^ ")"
  | PtList(ps) -> "PtList([" ^ (String.concat_map "; " string_of_pattern ps) ^ "])"
  | PtCons (p1, p2) ->
    Printf.sprintf "PtCons(%s, %s)" (string_of_pattern p1) (string_of_pattern p2)
  | PtRecord(xps) -> "PtRecord([" ^ (String.concat "; " (List.map (fun (x, p) -> x ^ ", " ^ (string_of_pattern p)) xps)) ^ "])"
  | PtConstr(x, ps, t) ->
    Printf.sprintf "PtConstr(%s, [%s], %s)"
      (Binding.to_string x)
      (String.concat_map "; " string_of_pattern ps)
      (Type.to_string t)

let rec string_of_typed_expr { With.Loc.desc = (e, t) } =
  (string_of_expr e) ^ " : " ^ (Type.to_string t)

and string_of_expr = 
  function
  | Unit -> "Unit"
  | Bool(b) -> "Bool(" ^ (string_of_bool b) ^ ")"
  | Int (b, v) -> Printf.sprintf "Int(%d, %s)" b v
  | Float v -> "Float(" ^ (string_of_float v) ^ ")"
  | Char s -> "Char(" ^ s ^ ")"
  | String s -> "String(" ^ s ^ ")"
  | Atom s -> "Atom(" ^ s ^ ")"
  | Bitstring x -> "Bitstring(" ^ (Bitstring.to_string x) ^ ")"
  | Record(xs) -> "Record(" ^ (String.concat_map "; " (fun (x, e) -> x ^ " = " ^ (string_of_typed_expr e)) xs) ^ ")"
  | Field(e, x) -> "Field(" ^ (string_of_typed_expr e) ^ ", " ^ x ^ ")"
  | List(es) -> "List([" ^ (String.concat "; " (List.map string_of_typed_expr es)) ^ "])"
  | Tuple(es) -> "Tuple([" ^ (String.concat "; " (List.map string_of_typed_expr es)) ^ "])"
  | Array(es) -> "Array([" ^ (String.concat "; " (List.map string_of_typed_expr es)) ^ "])"
  | Not(e) -> "Not(" ^ (string_of_typed_expr e) ^ ")"
  | And(e1, e2) -> "And(" ^ (string_of_typed_expr e1) ^ ", " ^ (string_of_typed_expr e2) ^ ")"
  | Or(e1, e2) -> "Or(" ^ (string_of_typed_expr e1) ^ ", " ^ (string_of_typed_expr e2) ^ ")"
  | Neg(e) -> "Neg(" ^ (string_of_typed_expr e) ^ ")"
  | Add(e1, e2) -> "Add(" ^ (string_of_typed_expr e1) ^ ", " ^ (string_of_typed_expr e2) ^ ")"
  | Sub(e1, e2) -> "Sub(" ^ (string_of_typed_expr e1) ^ ", " ^ (string_of_typed_expr e2) ^ ")"
  | Mul(e1, e2) -> "Mul(" ^ (string_of_typed_expr e1) ^ ", " ^ (string_of_typed_expr e2) ^ ")"
  | Div(e1, e2) -> "Div(" ^ (string_of_typed_expr e1) ^ ", " ^ (string_of_typed_expr e2) ^ ")"
  | Eq(e1, e2) -> "Eq(" ^ (string_of_typed_expr e1) ^ ", " ^ (string_of_typed_expr e2) ^ ")"
  | LE(e1, e2) -> "LE(" ^ (string_of_typed_expr e1) ^ ", " ^ (string_of_typed_expr e2) ^ ")"
  | If(e1, e2, e3) -> "If(" ^ (string_of_typed_expr e1) ^ " then " ^ (string_of_typed_expr e2) ^ " else " ^ (string_of_typed_expr e3) ^ ")"
  | Match(e, pes) -> "Match(" ^ (string_of_typed_expr e) ^ ", [" ^ (String.concat "; " (List.map (fun (p, e) -> (string_of_pattern p) ^ " -> " ^ (string_of_typed_expr e)) pes)) ^ "])"
  | LetVar((x, t), e1, e2) -> "LetVar(" ^ x ^ " : " ^ (Type.to_string t) ^ " = " ^ (string_of_typed_expr e1) ^ " in " ^ (string_of_typed_expr e2) ^ ")"
  | Var (`Unbound x) -> "Var(`Unbound " ^ (Binding.to_string x) ^ ")"
  | Var (`Local x) -> "Var(`Local " ^ x ^ ")"
  | Var (`Module x) -> "Var(`Module " ^ (Binding.to_string x) ^ ")"
  | Concat (e1, e2) -> "Concat(" ^ (string_of_typed_expr e1) ^ ", " ^ (string_of_typed_expr e2) ^ ")"
  | Constr(x, es) -> "Constr(" ^ (Binding.to_string x) ^ ", " ^ (String.concat_map ", " string_of_typed_expr es) ^ ")"
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> "LetRec(" ^ x ^ "(" ^ (String.concat ", " (List.map (fun (y, t) -> y ^ " : " ^ (Type.to_string t)) yts)) ^ ") : " ^ (Type.to_string t) ^ " = " ^ (string_of_typed_expr e1) ^ " in " ^ (string_of_typed_expr e2) ^ ")"
  | App(e, es) -> "App(" ^ (string_of_typed_expr e) ^ ", [" ^ (String.concat ", " (List.map string_of_typed_expr es)) ^ "])"
  | Get (e1, e2) ->
    Printf.sprintf "Get(%s, %s)" (string_of_typed_expr e1)
      (string_of_typed_expr e2)
  | Put (e1, e2, e3) ->
    Printf.sprintf "Put(%s, %s, %s)" (string_of_typed_expr e1)
      (string_of_typed_expr e2) (string_of_typed_expr e3)
  | Perform e -> Printf.sprintf "Perform(%s)" (string_of_typed_expr e)
  | Bind ((x, t), e) ->
    Printf.sprintf "Bind(%s:%s = %s)" x (Type.to_string t) (string_of_typed_expr e) 
  | Return e -> Printf.sprintf "Return(%s)" (string_of_typed_expr e)

let string_of_fundef { name = (x, t); args = yts; body = e } =
  x ^ " " ^ (String.concat " " (List.map (fun (y, t) -> y) yts)) ^ " : " ^ (Type.to_string t) ^ " = " ^ (string_of_typed_expr e) 

let string_of_sigdef { sig_name = (x, t); sig_ext = ext } =
  let typ = Type.to_string t in
  match ext with
  | None -> Printf.sprintf "%s : %s" x typ
  | Some f -> Printf.sprintf "external %s : %s = %s" x typ f

let string_of_def { With.Loc.desc = def } =
  match def with
  | Nop -> "Nop"
  | TypeDef(x, t) -> "TypeDef(" ^ x ^ ", " ^ (Type.Tycon.to_string t) ^ ")"
  | VarDef((x, t), e) -> "VarDef((" ^ x ^ ", " ^ (Type.to_string t) ^ "), " ^ (string_of_typed_expr e)
  | RecDef(fundef) -> "RecDef(" ^ (string_of_fundef fundef) ^ ")"
  | SigDef(sigdef) -> "SigDef(" ^ (string_of_sigdef sigdef) ^ ")"

let fold f defs env =
  let _, defs' =
    List.fold_left
      (fun ({ Env.venv = venv; tenv = tenv; tycons = tycons; mods = mods } as env, defs) def -> 
        match With.Loc.desc def with
        | TypeDef(x, t) -> 
            { Env.venv = M.add_list (Type.Tycon.vars t) venv;
              Env.tenv = M.add_list (Type.Tycon.types t) tenv;
              Env.tycons = M.add x t tycons;
              Env.mods = mods },
          f (env, defs) def
        | VarDef((x, t), e) -> 
            Env.add_var env x t, f (env, defs) def
        | RecDef({ name = (x, ty_f); args = yts; body = e }) -> 
            let env' = { env with Env.venv = M.add_list yts (M.add x ty_f venv) } in
            { env with Env.venv = M.add x ty_f venv }, f (env', defs) def
        | _ -> assert false)
      (env, []) defs in
  List.rev defs'

module Pattern = struct

  type t = pattern

  let to_string = string_of_pattern

  let fold ret f env es =
    let env', es' =
      List.fold_left
        (fun (env, accu) e ->
           let env', e' = f env e in
           env', e' :: accu)
        (env, []) es
    in
    env', ret & List.rev es'

  let fold_bin ret f env e1 e2 =
    fold (fun es -> ret (List.nth es 0) (List.nth es 1)) f env [e1; e2]

  let fold_assoc ret f env es =
    let env', es' =
      List.fold_left
        (fun (env, accu) (k, v) ->
           let env', v' = f env v in
           env', (k, v') :: accu)
        (env, []) es
    in
    env', ret & List.rev es'

end
