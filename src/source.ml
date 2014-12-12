type t = {
  path : string;
  mod_name : string;
  erl_name : string;
  erl_path : string;
  defs : Ast_t.def list;
}

let parse path =
  Log.verbose "# Parsing\n";
  let inchan = open_in path in
  let defs = Parser.prog Lexer.token (Lexing.from_channel inchan) in
  Log.debug "# %s\n" (String.concat ";\n " (List.map Ast.string_of_def defs));
  { path; mod_name = Utils.module_name path;
    erl_name = Utils.base path; erl_path = Utils.erl_path path;
    defs }
