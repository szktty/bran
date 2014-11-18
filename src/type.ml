exception Parse_error of Location.t * string

type t =
  | Unit
  | Bool
  | Int
  | Float
  | String
  | Fun of fun_ (* arguments are uncurried *)
  | Tuple of t list
  | List of t
  | Array of t
  | Var of t option ref
  | Module of module_
and module_ = {
  mod_name : string;
  mod_erl : string;
  mutable mod_funs : (string * fun_) list;
}
and fun_ = {
  fun_mod : module_ option;
  fun_ext : string option;
  fun_name : string option;
  fun_args : t list;
  fun_ret : t;
}

let rec to_string = function
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Fun f -> fun_to_string f
  | Tuple es ->
    Printf.sprintf "(%s)" (Xstring.concat_list " * " to_string es)
  | List e -> Printf.sprintf "%s list" (to_string e)
  | Array _ -> "a" 
  | Var _ -> "v"
  | Module m ->
    Printf.sprintf "module %s : sig %s end" m.mod_name
      (Xstring.concat_list "; "
         (fun (x, f) ->
            Printf.sprintf "val %s : %s" x (fun_to_string f))
         m.mod_funs)
and fun_to_string f =
  Printf.sprintf "%s%s -> %s%s"
    (match f.fun_ext with
     | None -> ""
     | Some _ -> "external ")
    (Xstring.concat_list " -> " to_string f.fun_args)
    (to_string f.fun_ret)
    (match f.fun_ext with
     | None -> ""
     | Some ext -> Printf.sprintf " = \"%s\"" ext)

let gentyp () = Var (ref None)
