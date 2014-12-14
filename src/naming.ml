open Base
open Locating
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

let rec resolve env mx { loc = loc; desc = (e, t) } =
  Log.debug "# Naming.resolve : %s\n" (Ast.string_of_expr e);
  let f = resolve env mx in
  let map = List.map f in
  let e', t' =
    match e with
    | Unit | Bool _ | Int _ | Float _ | Char _ | String _ | Atom _ | Bitstring _ -> e, t
    | Match (e1, ptns) ->
      Match (f e1, List.map (fun (p, e) -> p, f e) ptns), t
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
      LetVar ((x, t), f e1, resolve env' mx e2), t
    | LetRec ({ name = xt; args = yts; body = e1 }, e2) ->
      let env' = Env.add_vars env yts in
      LetRec ({ name = xt; args = yts; body = resolve env' mx e1 }, f e2), t
    | App (e, es) -> App (f e, map es), t
    | Perform e -> Perform (f e), t
    | Bind (xt, e) -> Bind (xt, f e), t
    | Return e -> Return (f e), t
  in
  create loc (e', t')

let resolve_def env mx def =
  Log.debug "# Naming.resolve_def: %s\n" (Ast.string_of_def def);
  set def & match def.desc with
  | TypeDef _ -> def.desc
  | VarDef (xt, et) -> VarDef (xt, resolve env mx et)
  | RecDef({ name = (x, ty_f); args = yts; body = et } as f) ->
    let env' = List.fold_left (fun env (x, t) -> Env.add_var env x t) env yts in
    RecDef { f with body = resolve env' mx et }
  | _ -> assert false

let f mx defs = 
  Ast.fold
    (fun (env, defs) def -> resolve_def env mx def :: defs)
    defs (Sig.create_env ())
