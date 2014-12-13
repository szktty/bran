open Base
open Locating
open Ast_t

exception Unbound_value_error of Location.t * Id.t * Id.t list
exception Unbound_module_error of Location.t * Id.t * Id.t list

let import env loc mx =
  match Library.find_module_opt mx with
  | None -> raise (Unbound_module_error (loc, Binding.to_string mx, []))
  | Some m -> List.fold_left (fun env (x, _) -> M.add x (Some mx) env) env m.vals

let resolve mx env e =
  (* TODO *)
  Log.debug "# Naming.resolve : %s\n" (Ast.string_of_typed_expr e);
  match e.desc with
  | _ -> e

let resolve_def (mx : Binding.t) env def =
  Log.debug "# Naming.resolve_def: %s\n" (Ast.string_of_def def);
  match def.desc with
  | TypeDef (x, t) ->
    (* TODO *)
    env, def
  | VarDef ((x, t), et) ->
    M.add x (Some mx) env, set def & VarDef ((x, t), resolve mx env et)
  | RecDef({ name = (x, ty_f); args = yts; body = et } as f) ->
    let env' = List.fold_left (fun env (x, _) -> M.add x None env) env yts in
    let body' = resolve mx env' et in
    M.add x (Some mx) env, set def & RecDef { f with body = body' }
  | _ -> assert false

let f mx defs = 
  let env = import M.empty Location.zero Binding.pervasives in
  let _, defs' =
    List.fold_left
      (fun (env, defs) def ->
         let env', def' = resolve_def mx env def in
           env', def' :: defs)
      (env, []) defs
  in
  List.rev defs'
