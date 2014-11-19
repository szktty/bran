%{
(* parserが利用する変数、関数、型などの定義 *)
open Syntax
let add_type x = (x, Type.Meta(Type.newmetavar ()))
let constr_args = function Tuple(xs), _ -> xs | x, t -> [(x, t)]
let constr_pattern_args = function PtTuple(xs) -> xs | x -> [x]

%}

/* 字句を表すデータ型の定義 (caml2html: parser_token) */
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token NOT
%token MINUS
%token PLUS
%token AST
%token SLASH
%token CONS
%token LAND
%token LOR
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token <Id.t> UIDENT
%token LET
%token IN
%token REC
%token TYPE
%token OF
%token MATCH
%token WITH
%token RIGHT_ARROW
%token SEMI
%token COLON
%token LPAREN
%token RPAREN
%token BEGIN
%token END
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token DOT
%token COMMA
%token PIPE
%token QUATE
%token EOF

/* 優先順位とassociativityの定義（低い方から高い方へ） (caml2html: parser_prior) */
%right SEMI
%right prec_if
%nonassoc tuple_ tuple_guard
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%right LAND
%right LOR
%right CONS
%left PLUS MINUS
%left AST SLASH
%right prec_unary_minus
%left prec_app

%nonassoc app
%nonassoc guard
%nonassoc PIPE
%nonassoc UIDENT LPAREN LBRACK INT IDENT BOOL BEGIN RPAREN END

/* 開始記号の定義 */
%type <Syntax.def list> f
%start f

%%

f: 
| definitions EOF { $1 }
;

definitions:
| /* empty */
    { [] }
| definition definitions 
    { $1 :: $2 }
;

definition:
| LET IDENT EQUAL seq_expr
    { VarDef(add_type $2, $4) }
| LET LPAREN RPAREN EQUAL seq_expr
    { VarDef((Id.gentmp (Type.prefix (Type.App(Type.Unit, []))), (Type.App(Type.Unit, []))), $5) }
| LET REC fundef
    { RecDef($3) }
| TYPE typedef    
    { $2 }

simple_expr: /* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) */
| LPAREN expr RPAREN
    { $2 }
| BEGIN expr END
    { $2 }
| LPAREN seq_expr RPAREN
    { $2 }
| BEGIN seq_expr END
    { $2 }
| LPAREN RPAREN
    { add_type Unit }
| BEGIN END
    { add_type Unit }
| BOOL
    { add_type (Bool($1)) }
| INT
    { add_type (Int($1)) }
| IDENT
    { add_type (Var($1)) }
| UIDENT
    { add_type (Constr($1, [])) }
| simple_expr DOT IDENT
    { add_type (Field($1, $3)) }
| LBRACK list RBRACK
    { List.fold_right (fun x xs -> add_type (Constr("Cons", [x; xs]))) $2 (add_type (Constr("Nil", []))) }
;
expr: /* 一般の式 (caml2html: parser_expr) */
| simple_expr
    { $1 }
| NOT expr
    %prec prec_app
    { add_type (Not($2)) }
| MINUS expr
    %prec prec_unary_minus
    { add_type (Neg($2)) }
| expr PLUS expr /* 足し算を構文解析するルール (caml2html: parser_add) */
    { add_type (Add($1, $3)) }
| expr MINUS expr
    { add_type (Sub($1, $3)) }
| expr AST expr
    { add_type (Mul($1, $3)) }
| expr SLASH expr
    { add_type (Div($1, $3)) }
| expr CONS expr
    { add_type (Constr("Cons", [$1; $3])) }
| expr LAND expr
    { add_type (And($1, $3)) }
| expr LOR expr
    { add_type (Or($1, $3)) }
| expr EQUAL expr
    { add_type (Eq($1, $3)) }
| expr LESS_GREATER expr
    { add_type (Not(add_type (Eq($1, $3)))) }
| expr LESS expr
    { add_type (Not(add_type (LE($3, $1)))) }
| expr GREATER expr
    { add_type (Not(add_type (LE($1, $3)))) }
| expr LESS_EQUAL expr
    { add_type (LE($1, $3)) }
| expr GREATER_EQUAL expr
    { add_type (LE($3, $1)) }
| tuple %prec tuple_
    { add_type (Tuple(List.rev $1)) }
| IF expr THEN expr ELSE expr
    %prec prec_if
    { add_type (If($2, $4, $6)) }
| expr actual_args
    %prec prec_app
    { add_type (App($1, $2)) }
| UIDENT simple_expr
    { add_type (Constr($1, constr_args $2)) }
| LBRACE fields RBRACE
    { add_type (Record($2)) }
| LET IDENT EQUAL seq_expr IN seq_expr
    { add_type (LetVar(add_type $2, $4, $6)) }
| LET REC fundef IN seq_expr
    { add_type (LetRec($3, $5)) }
| MATCH expr WITH pattern_matching
    { add_type (Match($2, $4)) }
| error
    { failwith
	(Printf.sprintf "parse error near characters %d-%d"
	   (Parsing.symbol_start ())
	   (Parsing.symbol_end ())) }
;

seq_expr: 
| expr %prec app
    { $1 }
| expr SEMI seq_expr
    { add_type (LetVar((Id.gentmp (Type.prefix (Type.App(Type.Unit, []))), (Type.App(Type.Unit, []))), $1, $3)) }
;    

tuple:
| tuple COMMA expr
    { $3 :: $1 }
| expr COMMA expr
    { [$3; $1] }
;

fundef:
| IDENT formal_args EQUAL seq_expr
    { { name = add_type $1; args = $2; body = $4 } }
;

formal_args:
| IDENT formal_args
    { add_type $1 :: $2 }
| IDENT
    { [add_type $1] }
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
    { ($1, $3) }
;

pattern_matching:
| opt_pipe pattern RIGHT_ARROW seq_expr pattern_matching_tail
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
    { PtBool($1) }
| INT
    { PtInt($1) }
(* TODO: FLOAT *)
| IDENT
    { PtVar($1, Type.Meta(Type.newmetavar ())) }
| tuple_pattern %prec tuple_guard
    { PtTuple(List.rev $1) }
| LBRACE field_patterns RBRACE
    { PtRecord(List.rev $2) }
| UIDENT 
    { PtConstr($1, []) }
| UIDENT pattern
    { PtConstr($1, constr_pattern_args $2) }
| pattern CONS pattern
    { PtConstr("Cons", [$1; $3]) }
| LBRACK list_pattern RBRACK
    { List.fold_right (fun x xs -> (PtConstr("Cons", [x; xs]))) $2 (PtConstr("Nil", [])) }
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
    { ($1, $3) }
;

typedef:
| type_params IDENT EQUAL IDENT
    { TypeDef($2, Type.TyFun($1, (Type.App(Type.NameTycon($4, ref None), [])))) }
| type_params IDENT EQUAL LBRACE field_decls RBRACE
    { TypeDef($2, Type.TyFun($1, (Type.App(Type.Record($2, List.map fst $5), List.map snd $5)))) }
| type_params IDENT EQUAL variant_decls
    { TypeDef($2, Type.TyFun($1, (Type.App(Type.Variant($2, $4), [])))) }
;
type_params:
| /* empty */
    { [] }
| type_param type_params_tail
    { $1 :: $2 }
;
type_param:
| QUATE IDENT
    { $2 }
;
type_params_tail:
| /* empty */
    { [] }
| COMMA type_param type_params_tail
    { $2 :: $3 }
;
type_expr:
| IDENT
    { Type.App(Type.NameTycon($1, ref None), []) }
| QUATE IDENT
    { Type.Var($2) }
| type_expr IDENT
    { Type.App(Type.NameTycon($2, ref None), [$1]) }
;
field_decls:
| field_decl field_decls_tail
    { $1 :: $2 }
;
field_decls_tail:
| /* empty */
    { [] }
| SEMI field_decl field_decls_tail
    { $2 :: $3 }
;
field_decl:
| IDENT COLON type_expr
    { ($1, $3) }
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
    { ($1, $2) }
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
    
    
