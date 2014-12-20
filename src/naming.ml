open Base
open Location.With
open Ast_t

exception Unbound_value_error of Location.t * Id.t * Id.t list
exception Unbound_constr_error of Location.t * Id.t * Id.t list
exception Unbound_module_error of Location.t * Id.t * Id.t list

let find_module env loc mx =
  (* TODO: nested module *)
  match Library.find_module_opt mx with
  | Some m -> m
  | None ->
    match Sig.load_module mx with
    | `Error -> raise (Unbound_module_error (loc, Binding.to_string mx, []))
    | `Ok -> Library.find_module mx

let find_val env loc path =
  match Binding.path_name path with
  | None, name ->
    begin match Env.find_module_of_val_opt env name with
      | Some m ->
        `Module (Binding.add (Module.path m) name, Module.find_val m name)
      | None ->
        begin match Env.find_var_opt env name with
          | None -> `Not_found name
          | Some t -> `Local (name, t)
        end
    end
  | Some path, name ->
    let m = find_module env loc path in
    match Module.find_val_opt m name with
    | None -> `Not_found name
    | Some t -> `Module (Binding.add (Module.path m) name, t)

let rec resolve_ptn mx env p =
  let open Ast.Pattern in
  let f = resolve_ptn mx in
  let env', p' =
    match p.desc with
    | PtUnit | PtBool _ | PtInt _ | PtFloat _ | PtAtom _ | PtString _ ->
      env, p.desc
    | PtVar (x, t) -> Env.add_var env x t, p.desc
    | PtAlias (p, x, t) ->
      let env', p' = resolve_ptn mx env p in
      Env.add_var env' x t, PtAlias (p', x, t)
    | PtTuple ps -> fold (fun ps -> PtTuple ps) f env ps
    | PtList ps -> fold (fun ps -> PtList ps) f env ps
    | PtCons (p1, p2) -> fold_bin (fun p1 p2 -> PtCons (p1, p2)) f env p1 p2
    | PtConstr (x, ps, _) ->
      begin match find_val env p.with_ x with
        | `Not_found name -> raise (Unbound_constr_error (p.with_, name, []))
        | `Local (name, t) ->
          fold (fun ps -> PtConstr (Binding.add mx name, ps, t)) f env ps
        | `Module (x', t) ->
          fold (fun ps -> PtConstr (x', ps, t)) f env ps
      end
    | _ ->
      Printf.printf "%s\n" (Ast.Pattern.to_string p);
      failwith "not implemented"
  in
  env', set p p'

let rec resolve mx env { with_ = loc; desc = (e, t) } =
  Log.debug "# Naming.resolve : %s\n" (Ast.string_of_expr e);
  let f = resolve mx env in
  let map = List.map f in
  let e', t' =
    match e with
    | Unit | Bool _ | Int _ | Float _ | Char _ | String _ | Atom _ | Bitstring _ -> e, t
    | Match (e1, ptns) ->
      Match (f e1, List.map
               (fun (p, e) ->
                  let env', p' = resolve_ptn mx env p in
                  let e' = resolve mx env' e in
                  p', e') ptns), t
    | Var (`Unbound x) ->
      begin match find_val env loc x with
        | `Not_found name -> raise (Unbound_value_error (loc, name, []))
        | `Local (name, t') -> Var (`Local name), t'
        | `Module (x', t') -> Var (`Module x'), t'
      end
    | Var (`Local _) | Var (`Module _) -> e, t
    | Constr (x, es) ->
      begin match find_val env loc x with
        | `Not_found name -> raise (Unbound_constr_error (loc, name, []))
        | `Local (name, t') ->
          Constr (Binding.add mx name, map es), t'
        | `Module (x', t') -> Constr (x', map es), t'
      end
    | List es -> List (map es), t
    | Tuple es -> Tuple (map es), t
    | Array es -> Array (map es), t
    | Not e -> Not (f e), t
    | Neg e -> Neg (f e), t
    | And (e1, e2) -> And (f e1, f e2), t
    | Or (e1, e2) -> Or (f e1, f e2), t
    | Add (e1, e2) -> Add (f e1, f e2), t
    | Sub (e1, e2) -> Sub (f e1, f e2), t
    | Mul (e1, e2) -> Mul (f e1, f e2), t
    | Div (e1, e2) -> Div (f e1, f e2), t
    | Eq (e1, e2) -> Eq (f e1, f e2), t
    | LE (e1, e2) -> LE (f e1, f e2), t
    | Concat (e1, e2) -> Concat (f e1, f e2), t
    | If (e1, e2, e3) -> If (f e1, f e2, f e3), t
    | Get (e1, e2) -> Get (f e1, f e2), t
    | Put (e1, e2, e3) -> Put (f e1, f e2, f e3), t
    | Record xes -> Record (List.map (fun (x, e) -> (x, f e)) xes), t
    | Field (e, x) -> Field (f e, x), t
    | LetVar ((x, t), e1, e2) ->
      let env' = Env.add_var env x t in
      LetVar ((x, t), f e1, resolve mx env' e2), t
    | LetRec ({ name = xt; args = yts; body = e1 }, e2) ->
      let env' = Env.add_vars env yts in
      LetRec ({ name = xt; args = yts; body = resolve mx env' e1 }, f e2), t
    | App (e, es) -> App (f e, map es), t
    | Perform e -> Perform (f e), t
    | Bind (xt, e) -> Bind (xt, f e), t
    | Return e -> Return (f e), t
  in
  create loc (e', t')

let resolve_def mx env def =
  Log.debug "# Naming.resolve_def: %s\n" (Ast.string_of_def def);
  set def & match def.desc with
  | TypeDef _ -> def.desc
  | VarDef (xt, et) -> VarDef (xt, resolve mx env et)
  | RecDef({ name = (x, ty_f); args = yts; body = et } as f) ->
    let env' = Env.add_vars env yts in
    RecDef { f with body = resolve mx env' et }
  | _ -> assert false

let f mx defs = 
  Ast.fold
    (fun (env, defs) def -> resolve_def mx env def :: defs)
    defs (Sig.create_env ())
