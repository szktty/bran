exception Syntax_error of Location.t

type t = (expr * Type.t) Locating.t
and expr = 
  | Unit
  | Bool of bool
  | Int of int
  | String of string
  | Record of (Id.t * t) list
  | Field of t * Id.t
  | Tuple of t list
  | Not of t
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
  | LetVar of (Id.t * Type.t) * t * t
  | Var of Id.t
  | Concat of t * t
  | Constr of Id.t * t list
  | LetRec of fundef * t
  | App of t * t list
and pattern = pattern_desc Locating.t
and pattern_desc =
  | PtBool of bool
  | PtInt of int
  | PtVar of Id.t * Type.t
  | PtTuple of pattern list
  | PtRecord of (Id.t * pattern) list
  | PtConstr of Id.t * pattern list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
and def = def_desc Locating.t
and def_desc =
  | TypeDef of Id.t * Type.tycon
  | VarDef of (Id.t * Type.t) * t
  | RecDef of fundef

let rec string_of_pattern { Locating.desc = p } =
  match p with
  | PtBool(b) -> "PtBool(" ^ (string_of_bool b) ^ ")"
  | PtInt(n) -> "PtInt(" ^ (string_of_int n) ^ ")"
  | PtVar(x, t) -> "PtVar(" ^ x ^ "," ^ (Type.string_of_t t) ^ ")"
  | PtTuple(ps) -> "PtTuple([" ^ (String.concat "; " (List.map string_of_pattern ps)) ^ "])"
  | PtRecord(xps) -> "PtRecord([" ^ (String.concat "; " (List.map (fun (x, p) -> x ^ ", " ^ (string_of_pattern p)) xps)) ^ "])"
  | PtConstr(x, ps) -> "PtConstr(" ^ x ^ ", [" ^ (String.concat "; " (List.map string_of_pattern ps)) ^ "])"

let rec string_of_typed_expr { Locating.desc = (e, t) } =
  (string_of_expr e) ^ " : " ^ (Type.string_of_t t)

and string_of_expr = 
  function
  | Unit -> "Unit"
  | Bool(b) -> "Bool(" ^ (string_of_bool b) ^ ")"
  | Int(n) -> "Int(" ^ (string_of_int n) ^ ")"
  | String s -> "String(" ^ s ^ ")"
  | Record(xs) -> "Record(" ^ (Xstring.concat_list "; " (fun (x, e) -> x ^ " = " ^ (string_of_typed_expr e)) xs) ^ ")"
  | Field(e, x) -> "Field(" ^ (string_of_typed_expr e) ^ ", " ^ x ^ ")"
  | Tuple(es) -> "Tuple([" ^ (String.concat "; " (List.map string_of_typed_expr es)) ^ "])"
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
  | LetVar((x, t), e1, e2) -> "LetVar(" ^ x ^ " : " ^ (Type.string_of_t t) ^ " = " ^ (string_of_typed_expr e1) ^ " in " ^ (string_of_typed_expr e2) ^ ")"
  | Var(x) -> "Var(" ^ x ^ ")"
  | Concat (e1, e2) -> "Concat(" ^ (string_of_typed_expr e1) ^ ", " ^ (string_of_typed_expr e2) ^ ")"
  | Constr(x, es) -> "Constr(" ^ x ^ ", " ^ (String.concat ", " (List.map string_of_typed_expr es)) ^ ")"
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> "LetRec(" ^ x ^ "(" ^ (String.concat ", " (List.map (fun (y, t) -> y ^ " : " ^ (Type.string_of_t t)) yts)) ^ ") : " ^ (Type.string_of_t t) ^ " = " ^ (string_of_typed_expr e1) ^ " in " ^ (string_of_typed_expr e2) ^ ")"
  | App(e, es) -> "App(" ^ (string_of_typed_expr e) ^ ", [" ^ (String.concat ", " (List.map string_of_typed_expr es)) ^ "])"

let string_of_fundef { name = (x, t); args = yts; body = e } =
  x ^ " " ^ (String.concat " " (List.map (fun (y, t) -> y) yts)) ^ " : " ^ (Type.string_of_t t) ^ " = " ^ (string_of_typed_expr e) 

let string_of_def { Locating.desc = def } =
  match def with
  | TypeDef(x, t) -> "TypeDef(" ^ x ^ ", " ^ (Type.string_of_tycon t) ^ ")"
  | VarDef((x, t), e) -> "VarDef((" ^ x ^ ", " ^ (Type.string_of_t t) ^ "), " ^ (string_of_typed_expr e)
  | RecDef(fundef) -> "RecDef(" ^ (string_of_fundef fundef) ^ ")"

let fold f defs =
  let _, defs' = 
    List.fold_left
      (fun ({ Env.venv = venv; tenv = tenv; tycons = tycons } as env, defs) def -> 
        match Locating.desc def with
        | TypeDef(x, t) -> 
            { Env.venv = M.add_list (Type.vars t) venv;
              Env.tenv = M.add_list (Type.types t) tenv;
              Env.tycons = M.add x t tycons }, 
          f (env, defs) def
        | VarDef((x, t), e) -> 
            Env.add_var env x t, f (env, defs) def
        | RecDef({ name = (x, ty_f); args = yts; body = e }) -> 
            let env' = { env with Env.venv = M.add_list yts (M.add x ty_f venv) } in
            { env with Env.venv = M.add x ty_f venv }, f (env', defs) def)
      (!Env.empty, []) defs in
  List.rev defs'
