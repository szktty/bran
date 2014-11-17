{
open Lexing
open Parser

module L = Location

exception Error of Location.t * string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = pos.pos_cnum;
               pos_lnum = pos.pos_lnum + 1
    }

let revise_pos pos lexbuf =
  Position.of_lexing_pos
    { pos with pos_lnum = pos.pos_lnum - 1;
               pos_bol = pos.pos_cnum - lexbuf.lex_curr_p.pos_bol }

let start_pos lexbuf =
  revise_pos (lexeme_start_p lexbuf) lexbuf

let end_pos lexbuf =
  revise_pos (lexeme_end_p lexbuf) lexbuf

let to_loc lexbuf =
  L.create (start_pos lexbuf) (end_pos lexbuf)

let to_word lexbuf =
  L.with_loc (to_loc lexbuf) (lexeme lexbuf)

let strlit_to_word lexbuf read =
  L.with_loc
    (L.create (start_pos lexbuf) (end_pos lexbuf))
    (read (Buffer.create 17) lexbuf)

}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let hex = '0' ['x' 'X']
let digit = ['0'-'9']
let body = (digit|lower|upper|['_' '\''])*
let ident = lower body
let uident = upper body
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let int = digit+ | hex hexdigit+
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let fnum = digit+ '.' digit* | ['.']? digit+
let hexfnum = hexdigit+ '.' hexdigit* | ['.']? hexdigit+
let binexp = ['p' 'P'] ['-' '+']? digit+
let float = fnum exp? | hex hexfnum binexp?
let white = [' ' '\t']+
let nl = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let hexstr = '\\' 'x' hexdigit hexdigit
let octstr = '\\' digit? digit? digit?
let escape = '\\' ['\'' '"' '\\' 'a' 'b' 'f' 'n' 'r' 't' 'v']
let dqstrchr = escape | hexstr | octstr | [^ '"' '\\' '\r' '\n']+
let sqstrchr = escape | hexstr | octstr | [^ '\'' '\\' '\r' '\n']+
let blank = [' ' '\t']*
let dirname = [^' ' '\t' '\r' '\n']+


rule token = parse
| blank
    { token lexbuf }
| nl
    { next_line lexbuf; NL (to_loc lexbuf) }
| "#"
    { comment lexbuf; (* ネストしたコメントのためのトリック *)
      token lexbuf }
| '('
    { LPAREN (to_loc lexbuf) }
| ')'
    { RPAREN (to_loc lexbuf) }
| '['
    { LBRACK (to_loc lexbuf) }
| ']'
    { RBRACK (to_loc lexbuf) }
| '{'
    { LBRACE (to_loc lexbuf) }
| '}'
    { RBRACE (to_loc lexbuf) }
| "true"
    { BOOL (L.with_loc (to_loc lexbuf) true) }
| "false"
    { BOOL (L.with_loc (to_loc lexbuf) false) }
| "not"
    { NOT (to_loc lexbuf) }
| digit+ (* 整数を字句解析するルール (caml2html: lexer_int) *)
    { INT (L.with_loc (to_loc lexbuf) (int_of_string (Lexing.lexeme lexbuf))) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT (L.with_loc (to_loc lexbuf) (float_of_string (Lexing.lexeme lexbuf))) }
| '-' (* -.より後回しにしなくても良い? 最長一致? *)
    { MINUS (to_loc lexbuf) }
| '+' (* +.より後回しにしなくても良い? 最長一致? *)
    { PLUS (to_loc lexbuf) }
| "-."
    { MINUS_DOT (to_loc lexbuf) }
| "+."
    { PLUS_DOT (to_loc lexbuf) }
| "*."
    { AST_DOT (to_loc lexbuf) }
| "/."
    { SLASH_DOT (to_loc lexbuf) }
| '='
    { EQ (to_loc lexbuf) }
| "<>"
    { LESS_GREATER (to_loc lexbuf) }
| "<="
    { LESS_EQ (to_loc lexbuf) }
| ">="
    { GREATER_EQ (to_loc lexbuf) }
| '<'
    { LESS (to_loc lexbuf) }
| '>'
    { GREATER (to_loc lexbuf) }
| '^'
    { UP (to_loc lexbuf) }
| "->"
    { RARROW (to_loc lexbuf) }
| "if"
    { IF (to_loc lexbuf) }
| "then"
    { THEN (to_loc lexbuf) }
| "else"
    { ELSE (to_loc lexbuf) }
| "elseif"
    { ELSEIF (to_loc lexbuf) }
| "let"
    { LET (to_loc lexbuf) }
| "in"
    { IN (to_loc lexbuf) }
| "rec"
    { REC (to_loc lexbuf) }
| "def" { DEF (to_loc lexbuf) }
| "external" { EXTERNAL (to_loc lexbuf) }
| "var" { VAR (to_loc lexbuf) }
| "where" { WHERE (to_loc lexbuf) }
| "import" { IMPORT (to_loc lexbuf) }
| "as" { AS (to_loc lexbuf) }
| "of" { OF (to_loc lexbuf) }
| "with" { WITH (to_loc lexbuf) }
| "match" { MATCH (to_loc lexbuf) }
| "end" { END (to_loc lexbuf) }
| "done" { DONE (to_loc lexbuf) }
| "for" { FOR (to_loc lexbuf) }
| "while" { WHILE (to_loc lexbuf) }
| "defer" { DEFER (to_loc lexbuf) }
| "raise" { RAISE (to_loc lexbuf) }
| "to" { TO (to_loc lexbuf) }
| "try" { TRY (to_loc lexbuf) }
| "type" { TYPE (to_loc lexbuf) }
| "module" { MODULE (to_loc lexbuf) }
| "and" { AND (to_loc lexbuf) }
| ','
    { COMMA (to_loc lexbuf) }
| '_'
    { IDENT (L.with_loc (to_loc lexbuf) (Id.gentmp Type.Unit)) }
| '.'
    { DOT (to_loc lexbuf) }
| "<-"
    { LESS_MINUS (to_loc lexbuf) }
| ':'
    { COLON (to_loc lexbuf) }
| ';'
    { SEMI (to_loc lexbuf) }
| '"'
    { STRING (strlit_to_word lexbuf string) }
| eof
    { EOF (to_loc lexbuf) }
| ident
    { IDENT (to_word lexbuf) }
| uident
    { UIDENT (to_word lexbuf) }
| _
    { raise (Error (to_loc lexbuf,
        Printf.sprintf "unknown token '%s'" (lexeme lexbuf))) }

and comment = parse
| nl+
    { () }
| eof
    { Format.eprintf "warning: unterminated comment." }
| _
    { comment lexbuf }

and string buf =
  parse
  | '"'       { Buffer.contents buf }
  | nl as s
    { Buffer.add_string buf s;
      next_line lexbuf;
      string buf lexbuf }
  | dqstrchr as s
    { Buffer.add_string buf s; string buf lexbuf }
  | _ { raise (Error (to_loc lexbuf, "Illegal string character: " ^ lexeme lexbuf)) }
  | eof { raise (Error (to_loc lexbuf, "String is not terminated")) }

