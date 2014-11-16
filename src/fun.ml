type t = Type.fun_

let create args ret =
  { Type.fun_mod = None; fun_ext = None; fun_name = None; fun_args = args; fun_ret = ret; }

let to_string t = Type.fun_to_string t

let to_typ t = Type.Fun t

let erl_sig t =
  let open Type in
  match t.fun_mod with
  | None -> assert false
  | Some m ->
    match t.fun_ext with
    | Some ext -> ext
    | None ->
      match t.fun_name with
      | None -> assert false
      | Some x -> m.mod_erl ^ ":" ^ x
