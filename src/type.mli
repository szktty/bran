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

val to_string : t -> string
val fun_to_string : fun_ -> string

val gentyp : unit -> t
