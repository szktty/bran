%{
open AstTypes

module L = Location

let addtyp x = (x, Type.gentyp ())

let combine es =
  (*let es' = List.filter (fun e -> (Ast.desc e) <> Nop) (List.rev es) in*)
  List.fold_right (fun e1 e2 ->
      match Ast.desc e2 with
      | Nop -> e1
      | _ ->
        match Ast.desc e1 with
        | Nop -> e2
        | Let (xt, e, _) ->
          L.with_range e1.L.loc e2.L.loc (Let (xt, e, e2))
        | _ ->
          let e3 = (Let ((Id.gentmp Type.Unit, Type.Unit), e1, e2)) in
          L.with_loc (Ast.loc e1) e3)
    es (L.with_loc L.zero Nop)

let find_funsig_opt x =
  Log.debug "# find funsig \"%s\":\n" x;
  match Context.current_module () with
  | None ->
    Log.debug "#   not found module\n";
    None
  | Some m ->
    begin try
      let (x, f) = List.find (fun (x', _) -> x = x') m.mod_funs in
      Log.debug "#   found: %s\n" (Fun.to_string f);
      Some (x, Type.Fun f)
    with Not_found ->
      Log.debug "#   not found funsig\n";
      None
    end

let find_funsig x =
  match find_funsig_opt x with
  | None -> addtyp x
  | Some xt -> xt

let sigdef_error loc t =
  raise (Type.Parse_error (loc,
    Printf.sprintf "type must be function: %s" (Type.to_string t)))

let bin_exp l op r =
  L.with_range l.L.loc r.L.loc (Bin (l, op, r))

%}

%token <bool Location.loc> BOOL
%token <int Location.loc> INT
%token <float Location.loc> FLOAT
%token <string Location.loc> STRING
%token <Location.t> NOT
%token <Location.t> MINUS
%token <Location.t> PLUS
%token <Location.t> MINUS_DOT
%token <Location.t> PLUS_DOT
%token <Location.t> AST_DOT
%token <Location.t> SLASH_DOT
%token <Location.t> EQ
%token <Location.t> LESS_GREATER
%token <Location.t> LESS_EQ
%token <Location.t> GREATER_EQ
%token <Location.t> LESS
%token <Location.t> GREATER
%token <Location.t> IF
%token <Location.t> THEN
%token <Location.t> ELSE
%token <Location.t> ELSEIF
%token <Id.t Location.loc> IDENT
%token <Id.t Location.loc> UIDENT
%token <Location.t> LET
%token <Location.t> DEF
%token <Location.t> DONE
%token <Location.t> END
%token <Location.t> EXTERNAL
%token <Location.t> FUN
%token <Location.t> IMPORT
%token <Location.t> AS
%token <Location.t> OF
%token <Location.t> WHERE
%token <Location.t> VAR
%token <Location.t> AND
%token <Location.t> IN
%token <Location.t> TO
%token <Location.t> WITH
%token <Location.t> REC
%token <Location.t> TYPE
%token <Location.t> MODULE
%token <Location.t> DEFER
%token <Location.t> RAISE
%token <Location.t> TRY
%token <Location.t> MATCH
%token <Location.t> FOR
%token <Location.t> WHILE
%token <Location.t> COLON
%token <Location.t> COMMA
%token <Location.t> DOT
%token <Location.t> LESS_MINUS
%token <Location.t> SEMI
%token <Location.t> BAR
%token <Location.t> UP
%token <Location.t> RARROW
%token <Location.t> LPAREN
%token <Location.t> RPAREN
%token <Location.t> LBRACK
%token <Location.t> RBRACK
%token <Location.t> LBRACE
%token <Location.t> RBRACE
%token <Location.t> NL
%token <Location.t> EOF

/* 優先順位とassociativityの定義（低い方から高い方へ） (caml2html: parser_prior) */
%right prec_let
%right SEMI
%right NL
%right prec_if
%right LESS_MINUS
%left COMMA
%left EQ LESS_GREATER LESS GREATER LESS_EQ GREATER_EQ
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST_DOT SLASH_DOT UP
%right prec_unary_minus
%left prec_app
%left DOT

/* 開始記号の定義 */
%type <Ast.t * Ast.t> prog
%start prog

%%

prog:
    | attrs EOF { $1 }

attrs:
    | rev_attrs { (combine (List.rev $1), List.hd $1) }

rev_attrs:
    | attr { [$1] }
    | rev_attrs attr { $2 :: $1 }

attr:
    | term { L.with_loc $1 Nop }
    | fundef { $1 }
    | sigdef { $1 }

term:
    | NL { $1 }
    | SEMI { $1 }

fundef:
    | DEF IDENT formal_args EQ body
      { L.with_range $1 $4
          (Def { AstTypes.name = find_funsig $2.desc;
                 rec_ = false; args = $3; body = $5 }) }
    | DEF IDENT formal_args COLON typexp EQ body
      { match find_funsig_opt $2.desc with
        | Some (_, t) ->
          raise (Type.Parse_error ($1,
            Printf.sprintf "signature already defined: %s" (Type.to_string t)))
        | None ->
          L.with_range $1 $4
          (Def { AstTypes.name = ($2.desc, $5);
                 rec_ = false; args = $3; body = $7 }) }
    | DEF REC IDENT formal_args EQ body
      { L.with_range $1 $5
          (Def { AstTypes.name = find_funsig $3.desc;
                 rec_ = true; args = $4; body = $6 }) }
    | DEF REC IDENT formal_args COLON typexp EQ body
      { match find_funsig_opt $3.desc with
        | Some (_, t) ->
          raise (Type.Parse_error ($1,
            Printf.sprintf "signature already defined: %s" (Type.to_string t)))
        | None ->
          L.with_range $1 $5
          (Def { AstTypes.name = ($3.desc, $6);
                 rec_ = true; args = $4; body = $8 }) }

formal_args:
    | formal_arg formal_args
      { $1 :: $2 }
    | formal_arg
      { [$1] }

formal_arg:
    | pattern { $1 }
    | LPAREN pattern RPAREN { $2 }

pattern:
    | IDENT { FunArg.Var (addtyp $1.desc) }
    | LPAREN IDENT COLON typexp RPAREN { FunArg.Var ($2.desc, $4) }
    | LPAREN RPAREN { FunArg.Var (Id.wildcard, Type.Unit) }
    | tuple_arg { $1 }

tuple_arg:
    | LPAREN tuple_arg_elt COMMA tuple_arg_elts RPAREN
      { FunArg.Tuple (Id.wildcard, $2 :: $4) }

tuple_arg_elts:
    | rev_tuple_arg_elts { List.rev $1 }

rev_tuple_arg_elts:
    | tuple_arg_elt { [$1] }
    | rev_tuple_arg_elts COMMA tuple_arg_elt { $3 :: $1 }

tuple_arg_elt:
    | formal_arg { $1 }

body:
    | stmts { combine $1 }

stmts:
    | rev_stmts { List.rev $1 }

rev_stmts:
    | stmt { [$1] }
    | rev_stmts stmt { $2 :: $1 }

stmt:
    | term { L.with_loc $1 Nop }
    | exp term { $1 }
    | if_exp { $1 }

if_exp:
    | IF exp THEN body END
      { L.with_range $1 $5 (If ($2, $4, L.with_loc $3 Unit)) }
    | IF exp THEN body ELSE body END
      { L.with_range $1 $7 (If ($2, $4, $6)) }

exp:
    | primary
      { $1 }
    | NOT exp %prec prec_app
      { L.with_range $1 $2.loc (Not $2) }
    | MINUS exp %prec prec_unary_minus
      { L.with_range $1 $2.loc
        (match $2.desc with
        | Float(f) -> Float(-.f) (* -1.23などは型エラーではないので別扱い *)
        | _ -> Neg $2) }
    | MINUS_DOT exp %prec prec_unary_minus
      { L.with_range $1 $2.loc (FNeg $2) }
    | bin_exp { $1 }
    | funcall { $1 }
    | vardef { $1 }
| error
    { failwith
	(Printf.sprintf "parse error near characters %d-%d"
	   (Parsing.symbol_start ())
	   (Parsing.symbol_end ())) }
   
vardef:
    | VAR IDENT EQ exp
      { L.with_range $1 $3 (Let (addtyp $2.desc, $4, L.with_loc $1 Nop)) }
    | VAR IDENT COLON typexp EQ exp
      { L.with_range $1 $3 (Let (($2.desc, $4), $6, L.with_loc $1 Nop)) }

bin_exp:
    | exp PLUS exp
      { bin_exp $1 BinOp.Add $3 }
    | exp MINUS exp
      { bin_exp $1 BinOp.Sub $3 }
    | exp PLUS_DOT exp
      { bin_exp $1 BinOp.FAdd $3 }
    | exp MINUS_DOT exp
      { bin_exp $1 BinOp.FSub $3 }
    | exp AST_DOT exp
      { bin_exp $1 BinOp.FMul $3 }
    | exp SLASH_DOT exp
      { bin_exp $1 BinOp.FDiv $3 }
    | exp UP exp
      { bin_exp $1 BinOp.SConcat $3 }
    | exp EQ exp
      { bin_exp $1 BinOp.Eq $3 }
    | exp LESS_GREATER exp
      { L.with_range $1.L.loc $3.L.loc (Not (bin_exp $1 BinOp.Eq $3)) }
    | exp LESS exp
      { bin_exp $1 BinOp.LT $3 }
    | exp GREATER exp
      { bin_exp $1 BinOp.GT $3 }
    | exp LESS_EQ exp
      { bin_exp $1 BinOp.LE $3 }
    | exp GREATER_EQ exp 
      { bin_exp $1 BinOp.GE $3 }

funcall:
    | primary actual_args
      { L.replace $1 (fun _ _ -> App ($1, $2)) }

actual_args:
    | actual_args actual_arg
      { $1 @ [$2] }
    | actual_arg
      { [$1] }

actual_arg:
    | primary { $1 }

primary:
    | LPAREN exp RPAREN
      { L.with_range $1 $3 $2.desc }
    | LPAREN exp COLON typexp RPAREN
      { L.with_range $1 $5 (Typed ($2, $4)) }
    | LPAREN RPAREN
      { L.with_range $1 $2 Unit }
    | BOOL
      { L.replace $1 (fun _ d -> Bool d) }
    | INT
      { L.replace $1 (fun _ d -> Int d) }
    | FLOAT
      { L.replace $1 (fun _ d -> Float d) }
    | STRING
      { L.replace $1 (fun _ d -> String d) }
    | var
      { $1 }
    | primary DOT LPAREN exp RPAREN
      { L.with_range $1.loc $5 (Get ($1, $4)) }
    | tuple
      { $1 }
    | list
      { $1 }

var:
    | IDENT
      { L.replace $1 (fun _ d -> Var d) }
    | UIDENT DOT var
      { L.replace $1 (fun _ d -> Local (d, $3)) }

tuple:
    | LPAREN tuple_elt COMMA tuple_elts RPAREN
      { L.with_range $1 $5 (Tuple ($2 :: $4)) }

tuple_elts:
    | rev_tuple_elts { List.rev $1 }

rev_tuple_elts:
    | tuple_elt { [$1] }
    | tuple_elts COMMA tuple_elt { $3 :: $1 }

tuple_elt:
    | exp { $1 }

list:
    | LBRACK RBRACK
      { L.with_range $1 $2 (List []) }
    | LBRACK list_elts RBRACK
      { L.with_range $1 $3 (List $2) }

list_elts:
    | rev_list_elts { List.rev $1 }

rev_list_elts:
    | list_elt { [$1] }
    | list_elts COMMA list_elt { $3 :: $1 }

list_elt:
    | exp { $1 }

sigdef:
    | DEF IDENT COLON typexp
      { let f = match $4 with
        | Type.Fun f -> f
        | _ -> sigdef_error $1 $4
        in
        L.with_range $1 $1 (SigDef ($2.desc, f))
      }
    | EXTERNAL IDENT COLON typexp EQ STRING
      { let f = match $4 with
        | Type.Fun f -> { f with fun_ext = Some $6.desc }
        | _ -> sigdef_error $1 $4
        in
        L.with_range $1 $6.loc (SigDef ($2.desc, f))
      }

typexp:
    | rev_typexp_elts
      { if List.length $1 = 1 then
          List.hd $1
        else
          let hd = List.hd $1 in
          let tl = List.tl $1 in
          Type.Fun { Type.fun_mod = None; fun_ext = None;
          fun_name = None; fun_args = List.rev tl; fun_ret = hd }
      }

rev_typexp_elts:
    | typexp_elt { [$1] }
    | rev_typexp_elts RARROW typexp_elt { $3 :: $1 }

typexp_elt:
    | typexp_elt_base { $1 }
    | typexp_elt_base IDENT
      { match $2.desc with
        (* TODO: option, ref *)
        | "list" -> Type.List $1
        | s -> raise (Type.Parse_error ($2.loc,
            (Printf.sprintf "type \"%s\" does not support" s)))
      }

typexp_elt_base:
    | IDENT
      { match $1.desc with
        | "unit" -> Type.Unit
        | "bool" -> Type.Bool
        | "int" -> Printf.printf "typexp int\n";Type.Int
        | "float" -> Type.Float
        | "string" -> Type.String
        | s -> raise (Type.Parse_error ($1.loc,
            (Printf.sprintf "type \"%s\" does not support" s)))
      }
    | LPAREN typexp RPAREN { $2 }
