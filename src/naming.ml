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
  (* TODO *)
  Log.debug "# Naming.resolve : %s\n" (Ast.string_of_expr e);
  let e', t' =
    match e with
    | Unit | Bool _ | Int _ | Float _ | Char _ | String _ | Atom _ | Bitstring _ -> e, t
    | Match (e1, ptns) ->
      Match (resolve env mx e1, List.map (fun (p, e) -> p, resolve env mx e) ptns), t
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
          Constr (Binding.add mx name, List.map (resolve env mx) es), t'
        | `Module (x', t') -> Constr (x', List.map (resolve env mx) es), t'
      end
        (*
  | Record of (Id.t * t) list
  | Field of t * Id.t
  | List of t list
  | Tuple of t list
  | Array of t list
  | Not e | 
  | And of t * t
  | Or of t * t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Match of t * (pattern * t) list
  | LetVar of (Id.t * Type_t.t) * t * t
  | Var of Binding.t ref
  | Concat of t * t
  | Constr of Binding.t ref * t list
  | LetRec of fundef * t
  | App of t * t list
  | Get of t * t
  | Put of t * t * t
  | Perform of t
  | Bind of (Id.t * Type_t.t) * t
  | Return of t
                                                                                     *)
    | _ -> e, t
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
(*val fold : (Env.t * def list -> def -> def list) -> def list -> Env.t -> def list*)
  Ast.fold
    (fun (env, defs) def -> resolve_def env mx def :: defs)
    defs (Sig.create_env ())
