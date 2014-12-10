%{
(* parserが利用する変数、関数、型などの定義 *)
open Base
open Ast_t
open Locating

let meta_type () = Type_t.Meta (Type.newmetavar ())

let add_type loc x =
  (x, create loc & meta_type ())

let add_type_loc x =
  add_type x.loc x.desc

let ast loc x =
  create loc & add_type loc x

let ast_on start end_ x =
  ast (Location.union start end_) x

let constr_args = function
  | { Locating.desc = (Tuple(xs), _) } -> xs
  | e -> [e]

let constr_pattern_args = function
  | { Locating.desc = PtTuple(xs) } -> xs
  | x -> [x]

let loc_of_list es =
  Location.union (List.hd es).loc (List.last es).loc

let combine e1 e2 =
  let typ = Type.app_unit e1.loc in
  ast_on e1.loc e2.loc
    (LetVar ((Id.gentmp (Type.prefix typ), typ), e1, e2))

let rev_combine_list = function
  | [] -> create Location.zero (Unit, Type.app_unit Location.zero)
  | init :: stmts ->
    List.fold_left (fun s1 s2 -> combine s2 s1) init stmts

%}

/* 字句を表すデータ型の定義 (caml2html: parser_token) */
%token <bool Locating.t> BOOL
%token <IntRepr.t Locating.t> INT
%token <float Locating.t> FLOAT
%token <string Locating.t> CHAR
%token <string Locating.t> STRING
%token <string Locating.t> ATOM
%token <Location.t> AS
%token <Location.t> ASSERT
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
%token <Location.t> LESS_LESS
%token <Location.t> GREATER_GREATER
%token <Location.t> ASSIGN (* := *)
%token <Location.t> EXCL (* ! *)
%token <Location.t> IF
%token <Location.t> THEN
%token <Location.t> ELSE
%token <Id.t Locating.t> IDENT
%token <Id.t Locating.t> UIDENT
%token <Id.t Locating.t> QIDENT (* 'a *)
%token <Location.t> DEF
%token <Location.t> TOPDEF
%token <Location.t> VAR
%token <Location.t> TOPVAR
%token <Location.t> EXTERNAL
%token <Location.t> IN
%token <Location.t> REC
%token <Location.t> TYPE
%token <Location.t> OF
%token <Location.t> TO
%token <Location.t> MATCH
%token <Location.t> MUTABLE
%token <Location.t> WITH
%token <Location.t> PERFORM
%token <Location.t> RETURN
%token <Location.t> RECEIVE
%token <Location.t> AND
%token <Location.t> MOD
%token <Location.t> LARROW (* <- *)
%token <Location.t> RARROW (* -> *)
%token <Location.t> UARROW (* ^ *)
%token <Location.t> SEMI
%token <Location.t> COLON
%token <Location.t> LPAREN
%token <Location.t> RPAREN
%token <Location.t> END
%token <Location.t> DO
%token <Location.t> FOR
%token <Location.t> FUN
%token <Location.t> RAISE
%token <Location.t> TRY
%token <Location.t> EXCEPTION
%token <Location.t> LBRACE
%token <Location.t> RBRACE
%token <Location.t> LBRACK
%token <Location.t> RBRACK
%token <Location.t> DOT
%token <Location.t> COMMA
%token <Location.t> PIPE
%token <Location.t> DOL (* $ *)
%token <Location.t> NL (* newline *)
%token <Location.t> EOF

/* 優先順位とassociativityの定義（低い方から高い方へ） (caml2html: parser_prior) */
%right prec_stmt
%nonassoc prec_simple_expr prec_mutual_def prec_constr_decl
%nonassoc AND
%right SEMI NL
%right DOL
%right LARROW
%left RARROW
%nonassoc prec_pattern
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%right LAND
%right LOR
%right UARROW
%right CONS
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST SLASH MOD AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%left DOT
%right UIDENT
%nonassoc INT FLOAT IDENT BOOL CHAR STRING ATOM LESS_LESS DO ASSIGN EXCL AS
%left LPAREN LBRACE LBRACK

%nonassoc prec_type_expr_tuple
%nonassoc RPAREN

/* 開始記号の定義 */
%type <Ast_t.def list> prog
%start prog

%%

prog: 
| definitions EOF { $1 }
;

definitions:
    | (* empty *)
      { [] }
    | rev_definitions
      { List.rev & List.filter (fun def -> def.desc <> Nop) $1 }

rev_definitions:
    | definition
      { [$1] }
    | rev_definitions definition
      { $2 :: $1 }

definition:
    | TOPVAR IDENT EQUAL nl_opt expr
      { range $1 $5.loc (VarDef (add_type_loc $2, $5)) }
    | TOPDEF fundef mutual_fundefs_opt
    (* TODO: mutual *)
      { create $1 (RecDef $2) }
    | TOPDEF REC fundef mutual_fundefs_opt
    (* TODO: mutual *)
      { create $1 (RecDef $3) }
    | TYPE typedef 
      { create $1 $2 }
    | AND typedef
    (* TODO: mutual *)
      { create $1 $2 }
    | EXCEPTION UIDENT
    (* TODO *)
      { create $1 Nop }
    | EXCEPTION UIDENT OF type_expr
    (* TODO *)
      { create $1 Nop }
    | EXCEPTION UIDENT EQUAL constr
    (* TODO *)
      { create $1 Nop }
    | TOPDEF sigdef
      { create $1 (SigDef $2) }
    | TOPVAR sigdef
    (* TODO *)
      { create $1 (SigDef $2) }
    | EXTERNAL ext_sigdef
      { create $1 (SigDef $2) }
    | NL
      { create $1 Nop }
    | error
      { raise (Syntax_error (Location.create
                             (Position.of_lexing_pos $startpos)
                             (Position.of_lexing_pos $endpos), None)) }

mutual_fundefs_opt:
    | (* empty *)
      %prec prec_mutual_def
      { [] }
    | rev_mutual_fundefs
      %prec prec_mutual_def
      { List.rev $1 }

rev_mutual_fundefs:
    | mutual_fundef
      { [$1] }
    | rev_mutual_fundefs NL mutual_fundef
      { $3 :: $1 }

mutual_fundef:
    | AND fundef { $2 }

simple_expr: /* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) */
    | primary { $1 }
    | field_expr { $1 }
    | array_expr { $1 }
    | EXCL simple_expr { $2 } (* TODO *)

primary:
    | binding
      { $1 }
    | LPAREN expr RPAREN
      { $2 }
    | LPAREN RPAREN
      { ast_on $1 $2 Unit }
    | BOOL
      { ast $1.loc (Bool $1.desc) }
    | INT
      { ast $1.loc (Int $1.desc) }
    | FLOAT
      { ast $1.loc (Float $1.desc) }
    | CHAR
      { ast $1.loc (Char $1.desc) }
    | STRING
      { ast $1.loc (String $1.desc) }
    | ATOM
      { ast $1.loc (Atom $1.desc) }
    | UIDENT
      { ast $1.loc (Constr($1.desc, [])) }
    | LBRACK list_ RBRACK
      { ast_on $1 $3 (List $2) }
    | LBRACK PIPE list_ PIPE RBRACK
      { ast_on $1 $5 (Array $3) }
    | LESS_LESS bitstring GREATER_GREATER
      { ast_on $1 $3 (Bitstring $2) }

binding:
    | value_name
      { $1 }
    | module_path value_name
      (* TODO: Ast.Binding *)
      { $2 }

value_name:
    | IDENT
      { ast $1.loc (Var $1.desc) }

module_path:
    | rev_module_path
      { List.rev $1 }

rev_module_path:
    | UIDENT DOT
      { [$1] }
    | rev_module_path UIDENT DOT
      { $2 :: $1 }

field_expr:
    | primary DOT binding
      (* TODO *)
      { $1 }

array_expr:
    | primary DOT LPAREN expr RPAREN
      { ast_on $1.loc $5 (Get ($1, $4)) }

expr:
    | simple_expr
      %prec prec_simple_expr
      { $1 }
    | NOT expr
      %prec prec_app
      { ast_on $1 $2.loc (Not $2) }
    | MINUS expr
      %prec prec_unary_minus
      { ast_on $1 $2.loc (Neg $2) }
    | expr PLUS expr
      { ast_on $1.loc $3.loc (Add ($1, $3)) }
    | expr MINUS expr
      { ast_on $1.loc $3.loc (Sub($1, $3)) }
    | expr AST expr
      { ast_on $1.loc $3.loc (Mul($1, $3)) }
    | expr SLASH expr
      { ast_on $1.loc $3.loc (Div($1, $3)) }
    | expr MOD expr
    (* TODO *)
      { ast_on $1.loc $3.loc (Div($1, $3)) }
    | expr PLUS_DOT expr
    (* TODO: FAdd *)
      { ast_on $1.loc $3.loc (Add($1, $3)) }
    | expr MINUS_DOT expr
    (* TODO: FSub *)
      { ast_on $1.loc $3.loc (Sub($1, $3)) }
    | expr AST_DOT expr
    (* TODO: FMul *)
      { ast_on $1.loc $3.loc (Mul($1, $3)) }
    | expr SLASH_DOT expr
    (* TODO: FDiv *)
      { ast_on $1.loc $3.loc (Div($1, $3)) }
    | expr UARROW expr
      { ast_on $1.loc $3.loc (Concat($1, $3)) }
    | expr CONS expr
      { ast_on $1.loc $3.loc (Constr("Cons", [$1; $3])) }
    | expr LAND expr
      { ast_on $1.loc $3.loc (And($1, $3)) }
    | expr LOR expr
      { ast_on $1.loc $3.loc (Or($1, $3)) }
    | expr EQUAL expr
      { ast_on $1.loc $3.loc (Eq($1, $3)) }
    | expr LESS_GREATER expr
      { let body = ast_on $1.loc $3.loc (Eq ($1, $3)) in
        ast_on $1.loc $3.loc (Not body) }
    | expr LESS expr
      { let body = ast_on $1.loc $3.loc (LE ($3, $1)) in
        ast_on $1.loc $3.loc (Not body) }
    | expr GREATER expr
      { let body = ast_on $1.loc $3.loc (LE ($1, $3)) in
        ast_on $1.loc $3.loc (Not body) }
    | expr LESS_EQUAL expr
      { ast_on $1.loc $3.loc (LE($1, $3)) }
    | expr GREATER_EQUAL expr
      { ast_on $1.loc $3.loc (LE($3, $1)) }
    | expr DOL expr
      { ast_on $1.loc $3.loc (App($1, [$3])) }
    | tuple   { $1 }
    | if_exp   { $1 }
    | expr actual_args
      %prec prec_app
      { ast_on $1.loc (List.last $2).loc (App($1, $2)) }
    | expr actual_args do_block
      { ast_on $1.loc $3.loc (App($1, $2)) }
    | UIDENT simple_expr
      { ast_on $1.loc $2.loc (Constr($1.desc, constr_args $2)) }
    | LBRACE fields RBRACE
      { ast_on $1 $3 (Record($2)) }
    | VAR IDENT EQUAL nl_opt expr term block
      { ast_on $1 $7.loc (LetVar(add_type_loc $2, $5, $7)) }
    | DEF fundef IN nl_opt block
      { ast_on $1 $5.loc (LetRec($2, $5)) }
    | DEF REC fundef IN nl_opt block
      { ast_on $1 $6.loc (LetRec($3, $6)) }
    | MATCH nl_opt expr WITH nl_opt pattern_matching END
      { ast_on $1 $7 (Match ($3, $6)) }
    | field_expr LARROW expr
    (* TODO *)
      { $1 }
    | array_expr LARROW expr
      { match $1.desc with
          | Get (e1, e2), _ ->
        ast_on $1.loc $3.loc (Put (e1, e2, $3))
          | _ -> assert false
    }
    | PERFORM nl_opt block END
      { ast_on $1 $4 (Perform $3)  }
    | IDENT LARROW expr
      { ast_on $1.loc $3.loc (Bind (add_type_loc $1, $3)) }
    | RETURN expr %prec prec_app
      { ast_on $1 $2.loc (Return $2) }
    | FOR IDENT EQUAL expr TO nl_opt expr nl_opt DO nl_opt block END
    (* TODO *)
      { ast_on $1 $12 Unit }
    | TRY nl_opt expr nl_opt WITH nl_opt pattern_matching END
    (* TODO *)
      { ast_on $1 $8 Unit }
    | RAISE expr %prec prec_app
    (* TODO *)
      { ast_on $1 $2.loc Unit }
    | FUN nl_opt rev_formal_args RARROW nl_opt block END
    (* TODO *)
      { ast_on $1 $7 Unit }
    | FUN nl_opt pattern_matching END
    (* TODO *)
      { ast_on $1 $1 Unit }
    | ASSERT expr %prec prec_app
    (* TODO *)
      { ast_on $1 $2.loc Unit }
    | IDENT ASSIGN nl_opt expr
    (* TODO *)
      { ast_on $1.loc $4.loc Unit }
    | field_expr ASSIGN nl_opt expr
    (* TODO *)
      { ast_on $1.loc $4.loc Unit }
    | RECEIVE nl_opt pattern_matching END
    (* TODO *)
      { ast_on $1 $4 Unit }

if_exp:
    | IF expr THEN nl_opt multi_exps_block END
      { let other = create $1 (Unit, Type.app_unit $1) in
        ast_on $1 $6 (If ($2, $5, other)) }
    | IF expr THEN nl_opt multi_exps_block ELSE nl_opt multi_exps_block END
      { ast_on $1 $9 (If ($2, $5, $8)) }
    | IF expr THEN nl_opt simple_expr nl_opt ELSE nl_opt simple_expr
      { ast_on $1 $9.loc (If ($2, $5, $9)) }

do_block:
    | DO nl_opt rev_formal_args RARROW nl_opt block END
      { create $1 (Unit, Type.app_unit) (* TODO *) }
    | DO nl_opt pattern_matching END
      { create $1 (Unit, Type.app_unit) (* TODO *) }

nl_opt:
    | (* empty *) %prec prec_constr_decl {}
    | NL {}

(* expand term (SEMI and NL) to solve reduce/reduce conflict *)
multi_exps_block:
    | rev_stmts SEMI expr { rev_combine_list ($3 :: $1) }
    | rev_stmts SEMI expr NL { rev_combine_list ($3 :: $1) }
    | rev_stmts NL expr { rev_combine_list ($3 :: $1) }
    | rev_stmts NL expr NL { rev_combine_list ($3 :: $1) }

block:
    | rev_stmts %prec prec_stmt { rev_combine_list $1 }
    | rev_stmts NL %prec prec_stmt { rev_combine_list $1 }

rev_stmts:
    | stmt { [$1] }
    | rev_stmts SEMI stmt { $3 :: $1 }
    | rev_stmts NL stmt { $3 :: $1 }

stmt:
    | expr %prec prec_stmt { $1 }

term:
    | SEMI {}
    | NL {}

tuple:
    | LPAREN rev_tuple RPAREN
      { ast_on $1 $3 (Tuple (List.rev $2)) }

rev_tuple:
    | rev_tuple COMMA expr
      { $3 :: $1 }
    | expr COMMA expr
      { [$3; $1] }

fundef:
    | IDENT rev_formal_args EQUAL nl_opt block
      (* convert argument patterns to pattern matching *)
      { let (_, args, body) = List.fold_left
          (fun (i, args, e1) (ptn, t) ->
             let x = "_t" ^ string_of_int i in
             let e2 = Match (create ptn.loc (Var x, t), [(ptn, e1)]) in
             (i + 1, (x, t) :: args, ast_on ptn.loc e1.loc e2))
          (0, [], $5) $2
        in
        { name = add_type_loc $1; args = args; body = body } }

rev_formal_args:
    | formal_arg
      { [add_type_loc $1] }
    | rev_formal_args formal_arg
      { add_type_loc $2 :: $1 }

formal_arg:
    | pattern
      %prec prec_pattern
      { create Location.zero $1 (* TODO: location *) }

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
    | rev_pattern_matching
      { List.rev $1 }

rev_pattern_matching:
    | rev_pattern_matching_elts { $1 }
    | PIPE rev_pattern_matching_elts { $2 }

rev_pattern_matching_elts:
    | pattern_matching_elt
      { [$1] }
    | rev_pattern_matching_elts PIPE pattern_matching_elt
      { $3 :: $1 }

pattern_matching_elt:
    | pattern RARROW nl_opt block
      { ($1, $4) }

pattern:
    | IDENT
      { create $1.loc (PtVar ($1.desc, create $1.loc & meta_type ())) }
    | LPAREN RPAREN
      { range $1 $2 PtUnit }
    | BOOL
      { create $1.loc (PtBool $1.desc) }
    | INT
      { create $1.loc (PtInt $1.desc) }
    | FLOAT
      (* TODO *)
      { create $1.loc PtUnit }
    | pattern AS value_name
      (* TODO *)
      { create $1.loc PtUnit }
    | LPAREN pattern RPAREN
      { $2 }
    | LPAREN pattern COLON type_expr RPAREN
      (* TODO *)
      { $2 }
    | LPAREN tuple_pattern RPAREN
      { range $1 $3 (PtTuple $2) }
    | LBRACE field_patterns RBRACE
      { range $1 $3 (PtRecord $2) }
    | UIDENT 
      { create $1.loc (PtConstr($1.desc, [])) }
    | UIDENT pattern
      { range $1.loc $2.loc (PtConstr($1.desc, constr_pattern_args $2)) }
    | LBRACK list_pattern RBRACK
      { List.fold_right (fun x xs ->
          create $1 (PtConstr("Cons", [x; xs])))
            $2 (create $3 (PtConstr("Nil", []))) }
    | pattern CONS pattern
      { range $1.loc $3.loc (PtConstr("Cons", [$1; $3])) }
    | LBRACK PIPE list_pattern PIPE RBRACK
      (* TODO *)
      { create $1 PtUnit }

tuple_pattern:
    | rev_tuple_pattern
      { List.rev $1 }

rev_tuple_pattern:
    | rev_tuple_pattern COMMA pattern
      { $3 :: $1 }
    | pattern COMMA pattern
      { [$3; $1] }

field_patterns:
    | field_pattern COMMA rev_field_patterns
      { List.rev ($1 :: $3) }
    | field_pattern COMMA rev_field_patterns COMMA
      { List.rev ($1 :: $3) }

rev_field_patterns:
    | field_pattern
      { [$1] }
    | rev_field_patterns COMMA field_pattern
      { $3 :: $1 }

field_pattern:
    | IDENT EQUAL pattern
      { ($1.desc, $3) }

(* TODO: include type_params in TypeDef *)
typedef:
    | type_params_opt IDENT EQUAL nl_opt type_expr
      { TypeDef($2.desc, Type_t.TyFun($1, $5)) }
    | type_params_opt IDENT EQUAL nl_opt constr_decls
      (* TODO *)
      { TypeDef($2.desc, Type_t.Variant ($2.desc, $5)) }
    | type_params_opt IDENT EQUAL nl_opt PIPE constr_decls
      (* TODO *)
      { TypeDef($2.desc, Type_t.Variant ($2.desc, $6)) }
    | type_params_opt IDENT EQUAL nl_opt LBRACE field_decls RBRACE
      (* TODO *)
      { TypeDef($2.desc, Type_t.Unit) }

type_params_opt:
    | (* empty *)
      { [] }
    | type_params
      { $1 }

type_params:
    | type_param
      { [$1] }
    | LPAREN rev_type_params RPAREN
      { List.rev $2 }

rev_type_params:
    | type_param
      { [$1] }
    | type_params COMMA type_param
      { $3 :: $1 }

type_param:
    | QIDENT
      { $1.desc }

type_expr:
    | simple_type_expr
      { $1 }
    | type_expr_tuple
      { $1 }
    | type_expr type_constr
      { range $1.loc $2.loc & Type_t.App ($2.desc, [$1]) }
    | type_expr RARROW type_expr
      { range $1.loc $3.loc & Type_t.App (Type_t.Arrow, [$1; $3]) }

simple_type_expr:
    | QIDENT
      { create $1.loc (Type_t.Var $1.desc) }
    | LPAREN type_expr RPAREN
      { range $1 $3 & Type_t.App (Type_t.Tuple, [$2]) }
    | type_constr
      { Type.void_app $1.loc $1.desc }
    | LPAREN type_constr_params RPAREN type_constr
      { range $1 $4.loc & Type_t.App ($4.desc, $2) }

type_expr_tuple:
    | simple_type_expr rev_type_expr_tuple_tail
      { let es = $1 :: List.rev $2 in
        create (loc_of_list es) (Type_t.App (Type_t.Tuple, es)) }

rev_type_expr_tuple_tail:
    | AST simple_type_expr
      { [$2] }
    | rev_type_expr_tuple_tail AST simple_type_expr
      { $3 :: $1 }

type_constr:
    | constr
      { create $1.loc & Type_t.NameTycon (Binding.to_string $1.desc, ref None) }

constr:
    | IDENT
      { create $1.loc & Binding.of_list [$1.desc] }
    | rev_constr_path IDENT
      { let es = concat & List.rev ($2 :: $1) in
        create es.loc & Binding.of_list es.desc }

rev_constr_path:
    | UIDENT
      { [$1] }
    | rev_constr_path DOT UIDENT
      { $3 :: $1 }

type_constr_params:
    | rev_type_constr_params { List.rev $1 }

rev_type_constr_params:
    | type_expr
      %prec prec_type_expr_tuple
      { [$1] }
    | rev_type_constr_params COMMA type_expr
      { $3 :: $1 }

constr_decls:
    | rev_constr_decls
      { List.rev $1 }

rev_constr_decls:
    | constr_decl
      { [$1] }
    | rev_constr_decls PIPE constr_decl
      { $3 :: $1 }

constr_decl:
    | UIDENT constr_decl_type nl_opt
      { ($1.desc, $2) }

constr_decl_type:
    | (* empty *)
      { [] }
    | OF type_expr
      { match $2.desc with
        | Type_t.App (Type_t.Tuple, es) -> es
        | _ -> [$2]
      }

field_decls:
    | rev_field_decls nl_opt { List.rev $1 }
    | NL rev_field_decls nl_opt { List.rev $2 }
    | rev_field_decls COMMA nl_opt { List.rev $1 }
    | NL rev_field_decls COMMA nl_opt { List.rev $2 }

rev_field_decls:
    | field_decl
      { [$1] }
    | rev_field_decls COMMA field_decl
      { $3 :: $1 }
    | rev_field_decls COMMA NL field_decl
      { $4 :: $1 }

field_decl:
    | IDENT COLON type_expr
      (* TODO *)
      { None }
    | MUTABLE IDENT COLON type_expr
      (* TODO *)
      { None }

list_: 
    | (* empty *)
      { [] }
    | rev_list_elts
      { List.rev $1 }
    | rev_list_elts COMMA
      { List.rev $1 }

rev_list_elts:
    | expr
      { [$1] }
    | rev_list_elts COMMA expr
      { $3 :: $1 }

list_pattern: 
    | (* empty *) { [] }
    | rev_list_pattern_elts { List.rev $1 }
    | rev_list_pattern_elts COMMA { List.rev $1 }

rev_list_pattern_elts:
    | pattern
      { [$1] }
    | rev_list_pattern_elts COMMA pattern
      { $3 :: $1 }

bitstring:
    | (* empty *)
      { [] }
    | rev_bitstring
      { List.rev $1 }

rev_bitstring:
    | segment
      { [$1] }
    | rev_bitstring COMMA segment
      { $3 :: $1 }

segment:
    | bits_value
      { Bitstring.Bits.create $1 }
    | bits_value COLON INT
      { Bitstring.Bits.create $1 ~size:(IntRepr.to_int $3.desc) }
    | bits_value COLON INT SLASH bits_spec_list
      { { $5 with Bitstring.Bits.value = $1;
                  size = Some (IntRepr.to_int $3.desc) } }
    | bits_value SLASH bits_spec_list
      { { $3 with Bitstring.Bits.value = $1; } }

bits_value:
    | INT { Bitstring.Bits.Int (IntRepr.to_int $1.desc) }
    | FLOAT { Bitstring.Bits.Float $1.desc }
    | STRING { Bitstring.Bits.String $1.desc }
    | IDENT { Bitstring.Bits.Var $1.desc }

bits_spec_list:
    | rev_bits_spec_list
      { let open Bitstring.Bits in
        List.fold_left
          (fun v spec ->
             match spec with
             | `Int -> { v with typ = `Int }
             | `Signed_int -> { v with typ = `Int; sign = Some `Signed }
             | `Float -> { v with typ = `Float }
             | `Binary -> { v with typ = `Binary }
             | `Bitstring -> { v with typ = `Bitstring }
             | `UTF8 -> { v with typ = `UTF8 }
             | `UTF16 -> { v with typ = `UTF16 }
             | `UTF32 -> { v with typ = `UTF32 }
             | `Signed -> { v with sign = Some `Signed }
             | `Unsigned -> { v with sign = Some `Unsigned }
             | `Big -> { v with endian = Some `Big }
             | `Little -> { v with endian = Some `Little }
             | `Native -> { v with endian = Some `Native }
             | `Unit size -> { v with unit = Some size })
          (create (Int 0)) $1
      }

rev_bits_spec_list:
    | bits_spec
      { [$1] }
    | rev_bits_spec_list MINUS bits_spec
      { $3 :: $1 }

bits_spec:
    | IDENT
      { match $1.desc with
        | "int" -> `Int
        | "integer" -> `Int
        | "sint" -> `Signed_int
        | "float" -> `Float
        | "binary" -> `Binary
        | "bytes" -> `Binary
        | "bitstring" -> `Bitstring
        | "bits" -> `Bitstring
        | "utf8" -> `UTF8
        | "utf16" -> `UTF16
        | "utf32" -> `UTF32
        | "big" -> `Big
        | "little" -> `Little
        | "native" -> `Native
        | "signed" -> `Signed
        | "unsigned" -> `Unsigned
        | _ -> raise (Syntax_error ($1.loc, Some ("Unknown type " ^ $1.desc)))
      }
    | IDENT COLON INT
      { match $1.desc with
        | "unit" -> `Unit (IntRepr.to_int $3.desc)
        | _ -> raise (Syntax_error ($1.loc, Some ("Unknown type " ^ $1.desc)))
      }

sigdef:
    | IDENT COLON type_expr
      { { sig_name = ($1.desc, $3); sig_ext = None } }

ext_sigdef:
    | sigdef EQUAL nl_opt STRING
      { { $1 with sig_ext = Some $4.desc } }
