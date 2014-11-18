type t = AstTypes.t

let desc t = t.Location.desc

let loc t = t.Location.loc

let rec to_string t =
  let open Printf in
  let lst ts =
    sprintf "%d:[%s]" (List.length ts)
      (Xstring.concat_list " " to_string ts)
  in
  let open AstTypes in
  match desc t with
  | Nop -> "Nop"
  | Unit -> "Unit"
  | Bool _ -> "Bool"
  | Int v -> sprintf "%d" v
  | Float v -> sprintf "%f" v
  | String v -> sprintf "\"%s\"" v
  | Not _ -> "Not"
  | Neg t -> sprintf "-%s" (to_string t)
  | If (t1, t2, t3) ->
    sprintf "(If %s %s %s)" (to_string t1) (to_string t2) (to_string t3)
  | Bin (l, op, r) ->
    sprintf "(%s %s %s)" (to_string l) (BinOp.to_string op) (to_string r)
  | Let ((id_, _typ), t1, t2) -> sprintf "(Let %s %s %s)" id_ (to_string t1) (to_string t2)
  | Var s -> sprintf "$%s" s
  | Local (s, t) -> sprintf "%s.(%s)" s (to_string t)
  | Def def ->
    let (name, _) = def.name in
    sprintf "(Def %s %s)" name (to_string def.body)
  | App (t, ts) -> sprintf "(App %s %s)" (to_string t) (lst ts)
  | Tuple ts -> sprintf "(Tuple %s)" (lst ts)
  | List ts -> sprintf "(List %s)" (lst ts)
  | Typed (t, typ) -> sprintf "(%s:%s)" (to_string t) (Type.to_string typ)
  | _ -> "Unknown"
