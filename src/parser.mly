%{
(* parserが利用する変数、関数、型などの定義 *)
open Syntax
open Locating

let add_type x = (x, Type.Meta(Type.newmetavar ()))

let constr_args = function
  | { Locating.desc = (Tuple(xs), _) } -> xs
  | e -> [e]

let constr_pattern_args = function
  | { Locating.desc = PtTuple(xs) } -> xs
  | x -> [x]

let range_from_list es desc =
  range (List.hd es).loc (Spotlib.Xlist.last es).loc desc

let rev_combine = function
  | [] -> create Location.zero (Unit, Type.app_unit)
  | init :: stmts ->
    List.fold_left (fun s1 s2 ->
        range s2.loc s1.loc
          (add_type
             (LetVar((Id.gentmp (Type.prefix Type.app_unit),
                      Type.app_unit), s2, s1))))
    init stmts

%}

/* 字句を表すデータ型の定義 (caml2html: parser_token) */
%token <bool Locating.t> BOOL
%token <int Locating.t> INT
%token <float Locating.t> FLOAT
%token <string Locating.t> STRING
%token <Location.t> NOT
%token <Location.t> MINUS
%token <Location.t> MINUS_DOT
%token <Location.t> PLUS
%token <Location.t> PLUS_DOT
%token <Location.t> AST
%token <Location.t> AST_DOT
%token <Location.t> SLASH
%token <Location.t> SLASH_DOT
%token <Location.t> CONS
%token <Location.t> LAND
%token <Location.t> LOR
%token <Location.t> EQUAL
%token <Location.t> LESS_GREATER
%token <Location.t> LESS_EQUAL
%token <Location.t> GREATER_EQUAL
%token <Location.t> LESS
%token <Location.t> GREATER
%token <Location.t> IF
%token <Location.t> THEN
%token <Location.t> ELSE
%token <Id.t Locating.t> IDENT
%token <Id.t Locating.t> UIDENT
%token <Location.t> DEF
%token <Location.t> VAR
%token <Location.t> EXTERNAL
%token <Location.t> IMPORT
%token <Location.t> MODULE
%token <Location.t> AS
%token <Location.t> IN
%token <Location.t> REC
%token <Location.t> TYPE
%token <Location.t> OF
%token <Location.t> TO
%token <Location.t> MATCH
%token <Location.t> WITH
%token <Location.t> AND
%token <Location.t> OR
%token <Location.t> LARROW (* <- *)
%token <Location.t> RARROW (* -> *)
%token <Location.t> UARROW (* ^ *)
%token <Location.t> SEMI
%token <Location.t> COLON
%token <Location.t> LPAREN
%token <Location.t> RPAREN
%token <Location.t> BEGIN
%token <Location.t> END
%token <Location.t> DONE
%token <Location.t> FOR
%token <Location.t> WHILE
%token <Location.t> DEFER
%token <Location.t> RAISE
%token <Location.t> TRY
%token <Location.t> LBRACE
%token <Location.t> RBRACE
%token <Location.t> LBRACK
%token <Location.t> RBRACK
%token <Location.t> DOT
%token <Location.t> COMMA
%token <Location.t> PIPE
%token <Location.t> QUATE
%token <Location.t> NL (* newline *)
%token <Location.t> EOF

/* 優先順位とassociativityの定義（低い方から高い方へ） (caml2html: parser_prior) */
%right prec_stmt
%right SEMI NL
%nonassoc tuple_ tuple_guard
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%right LAND
%right LOR
%right UARROW
%right CONS
%left PLUS MINUS
%left AST SLASH
%right prec_unary_minus
%left prec_app
%nonassoc guard
%nonassoc PIPE
%nonassoc UIDENT LPAREN LBRACK INT IDENT BOOL STRING BEGIN RPAREN END

/* 開始記号の定義 */
%type <Syntax.def list> prog
%start prog

%%

prog: 
| definitions EOF { $1 }
;

definitions:
    | (* empty *)
      { [] }
    | rev_definitions
      { List.rev $1 }

rev_definitions:
    | definition
      { [$1] }
    | rev_definitions definition
      { $2 :: $1 }

definition:
| VAR IDENT EQUAL nl_opt block
    { range $1 $5.loc (VarDef(add_type $2.desc, $5)) }
| VAR LPAREN RPAREN EQUAL nl_opt block
    { range $1 $4 (VarDef((Id.gentmp (Type.prefix (Type.App(Type.Unit, []))), (Type.App(Type.Unit, []))), $6)) }
| DEF fundef
    { create $1 (RecDef $2) }
| DEF REC fundef
    { create $1 (RecDef $3) }
| TYPE typedef    
    { create $1 $2 }
| NL
    { create $1 (VarDef((Id.gentmp (Type.prefix (Type.App(Type.Unit, []))), (Type.App(Type.Unit, []))), create $1 (Unit, Type.App(Type.Unit, [])))) }
| error
    { raise (Syntax_error (Location.create
                             (Position.of_lexing_pos $startpos)
                             (Position.of_lexing_pos $endpos))) }

simple_expr: /* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) */
| LPAREN expr RPAREN
    { $2 }
| BEGIN expr END
    { $2 }
| LPAREN block RPAREN
    { $2 }
| BEGIN block END
    { $2 }
| LPAREN RPAREN
    { range $1 $2 (add_type Unit) }
| BEGIN END
    { range $1 $2 (add_type Unit) }
| BOOL
    { create $1.loc (add_type (Bool $1.desc)) }
| INT
    { create $1.loc (add_type (Int $1.desc)) }
| STRING
    { create $1.loc (add_type (String $1.desc)) }
| IDENT
    { create $1.loc (add_type (Var $1.desc)) }
| UIDENT
    { create $1.loc (add_type (Constr($1.desc, []))) }
| simple_expr DOT IDENT
    { range $1.loc $3.loc (add_type (Field($1, $3.desc))) }
| LBRACK list RBRACK
    { List.fold_right (fun x xs ->
        create x.loc (add_type (Constr("Cons", [x; xs]))))
        $2 (create $3 (add_type (Constr("Nil", [])))) }

expr: /* 一般の式 (caml2html: parser_expr) */
| simple_expr
    { $1 }
| NOT expr %prec prec_app
    { range $1 $2.loc (add_type (Not($2))) }
| MINUS expr %prec prec_unary_minus
    { range $1 $2.loc (add_type (Neg($2))) }
| expr PLUS expr /* 足し算を構文解析するルール (caml2html: parser_add) */
    { range $1.loc $3.loc (add_type (Add($1, $3))) }
| expr MINUS expr
    { range $1.loc $3.loc (add_type (Sub($1, $3))) }
| expr AST expr
    { range $1.loc $3.loc (add_type (Mul($1, $3))) }
| expr SLASH expr
    { range $1.loc $3.loc (add_type (Div($1, $3))) }
| expr UARROW expr
    { range $1.loc $3.loc (add_type (Concat($1, $3))) }
| expr CONS expr
    { range $1.loc $3.loc (add_type (Constr("Cons", [$1; $3]))) }
| expr LAND expr
    { range $1.loc $3.loc (add_type (And($1, $3))) }
| expr LOR expr
    { range $1.loc $3.loc (add_type (Or($1, $3))) }
| expr EQUAL expr
    { range $1.loc $3.loc (add_type (Eq($1, $3))) }
| expr LESS_GREATER expr
    { let body = range $1.loc $3.loc (add_type (Eq ($1, $3))) in
      range $1.loc $3.loc (add_type (Not body)) }
| expr LESS expr
    { let body = range $1.loc $3.loc (add_type (LE ($3, $1))) in
      range $1.loc $3.loc (add_type (Not body)) }
| expr GREATER expr
    { let body = range $1.loc $3.loc (add_type (LE ($1, $3))) in
      range $1.loc $3.loc (add_type (Not body)) }
| expr LESS_EQUAL expr
    { range $1.loc $3.loc (add_type (LE($1, $3))) }
| expr GREATER_EQUAL expr
    { range $1.loc $3.loc (add_type (LE($3, $1))) }
| tuple %prec tuple_
    { range_from_list $1 (add_type (Tuple (List.rev $1))) }
| if_exp { $1 }
| expr actual_args
    %prec prec_app
    { range $1.loc (Spotlib.Xlist.last $2).loc (add_type (App($1, $2))) }
| UIDENT simple_expr
    { range $1.loc $2.loc (add_type (Constr($1.desc, constr_args $2))) }
| LBRACE fields RBRACE
    { range $1 $3 (add_type (Record($2))) }
| VAR IDENT EQUAL nl_opt block IN nl_opt block
    { range $1 $8.loc (add_type (LetVar(add_type $2.desc, $5, $8))) }
| DEF REC fundef IN nl_opt block
    { range $1 $6.loc (add_type (LetRec($3, $6))) }
| MATCH expr WITH nl_opt pattern_matching
    { create $1 (add_type (Match($2, $5))) }

if_exp:
    | IF expr THEN nl_opt block ELSE nl_opt block END
    { range $1 $8.loc (add_type (If($2, $5, $8))) }

nl_opt:
    | (* empty *) {}
    | nl {}

nl:
    | NL {}
    | nl NL {}

block:
    | rev_stmts %prec prec_stmt { rev_combine $1 }

rev_stmts:
    | stmt { [$1] }
    | rev_stmts terms stmt { $3 :: $1 }

stmt:
    | expr %prec prec_stmt { $1 }

terms:
    | term {}
    | terms term {}

term:
    | SEMI {}
    | NL {}

tuple:
| tuple COMMA expr
    { $3 :: $1 }
| expr COMMA expr
    { [$3; $1] }
;

fundef:
| IDENT formal_args EQUAL nl_opt block
    { { name = add_type $1.desc; args = $2; body = $5 } }
;

formal_args:
| IDENT formal_args
    { add_type $1.desc :: $2 }
| IDENT
    { [add_type $1.desc] }
;

actual_args:
| actual_args simple_expr
    { $1 @ [$2] }
| simple_expr
    { [$1] }
;
fields:
| field fields_tail
    { $1 :: $2 }
;
fields_tail:
| /* empty */
    { [] }
| SEMI field fields_tail
    { $2 :: $3 }
;
field:
| IDENT EQUAL expr
    { ($1.desc, $3) }
;

pattern_matching:
| opt_pipe pattern RARROW block pattern_matching_tail
    { ($2, $4) :: $5 }
;
pattern_matching_tail:
| /* empty */ %prec guard
    { [] }
| PIPE pattern_matching
    { $2 }
;
opt_pipe:
| /* empty */ 
    {  }
| PIPE
    {  }
;
pattern:
| LPAREN pattern RPAREN
    { $2 }
| BEGIN pattern END
    { $2 }
| BOOL
    { create $1.loc (PtBool $1.desc) }
| INT
    { create $1.loc (PtInt $1.desc) }
(* TODO: FLOAT *)
| IDENT
    { create $1.loc (PtVar($1.desc, Type.Meta(Type.newmetavar ()))) }
| tuple_pattern %prec tuple_guard
    { range (List.hd $1).loc (Spotlib.Xlist.last $1).loc (PtTuple(List.rev $1)) }
| LBRACE field_patterns RBRACE
    { range $1 $3 (PtRecord(List.rev $2)) }
| UIDENT 
    { create $1.loc (PtConstr($1.desc, [])) }
| UIDENT pattern
    { range $1.loc $2.loc (PtConstr($1.desc, constr_pattern_args $2)) }
| pattern CONS pattern
    { range $1.loc $3.loc (PtConstr("Cons", [$1; $3])) }
| LBRACK list_pattern RBRACK
    { List.fold_right (fun x xs ->
      create $1 (PtConstr("Cons", [x; xs])))
    $2 (create $3 (PtConstr("Nil", []))) }
;

tuple_pattern:
| tuple_pattern COMMA pattern
    { $3 :: $1 }
| pattern COMMA pattern
    { [$3; $1] }
;

field_patterns:
| field_patterns SEMI field_pattern
    { $3 :: $1 }
| field_pattern SEMI field_pattern
    { [$3; $1] }
;

field_pattern:
| IDENT EQUAL pattern
    { ($1.desc, $3) }
;

typedef:
| type_params IDENT EQUAL nl_opt IDENT
    { TypeDef($2.desc, Type.TyFun($1, (Type.App(Type.NameTycon($5.desc, ref None), [])))) }
| type_params IDENT EQUAL nl_opt LBRACE nl_opt field_decls RBRACE
    { TypeDef($2.desc, Type.TyFun($1, (Type.App(Type.Record($2.desc, List.map fst $7), List.map snd $7)))) }
| type_params IDENT EQUAL nl_opt variant_decls
    { TypeDef($2.desc, Type.TyFun($1, (Type.App(Type.Variant($2.desc, $5), [])))) }
;
type_params:
| /* empty */
    { [] }
| type_param type_params_tail
    { $1 :: $2 }
;
type_param:
| QUATE IDENT
    { $2.desc }
;
type_params_tail:
| /* empty */
    { [] }
| COMMA type_param type_params_tail
    { $2 :: $3 }
;
type_expr:
| IDENT
    { Type.App(Type.NameTycon($1.desc, ref None), []) }
| QUATE IDENT
    { Type.Var($2.desc) }
| type_expr IDENT
    { Type.App(Type.NameTycon($2.desc, ref None), [$1]) }
;
field_decls:
| field_decl field_decls_tail
    { $1 :: $2 }
;
field_decls_tail:
| /* empty */
    { [] }
| terms field_decl field_decls_tail
    { $2 :: $3 }
;
field_decl:
| IDENT COLON type_expr
    { ($1.desc, $3) }
;
variant_decls:
| variant_decl variant_decls_tail
    { $1 :: $2 }
;
variant_decls_tail:
| /* empty */
    { [] }
| PIPE variant_decl variant_decls_tail
    { $2 :: $3 }
;
variant_decl:
| UIDENT variant_var_decls
    { ($1.desc, $2) }
;
variant_var_decls:
| /* empty */
    { [] }
| OF type_expr variant_var_decls_tail
    { $2::$3 }
;
variant_var_decls_tail:
| /* empty */
    { [] }
| AST type_expr variant_var_decls_tail
    { $2::$3 }
;
list: 
| /* empty */
    { [] }
| expr tail
    { $1 :: $2 }
;
tail: 
| 
    { [] }
| SEMI expr tail
    { $2 :: $3 }
;
list_pattern: 
| /* empty */
    { [] }
| pattern tail_pattern
    { $1 :: $2 }
;
tail_pattern: 
| 
    { [] }
| SEMI pattern tail_pattern
    { $2 :: $3 }
    
    
