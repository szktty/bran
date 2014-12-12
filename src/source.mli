type t = {
  path : string;
  mod_name : string;
  erl_name : string;
  erl_path : string;
  defs : Ast_t.def list;
}

val parse : string -> t
