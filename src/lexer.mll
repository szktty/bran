{
open Lexing
open Parser
open Spotlib.Base
open X

exception Error of Location.t * string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = 0;
               pos_lnum = pos.pos_lnum + 1
    }

let next_line_in_spaces lexbuf s =
    let lines =  String.lines s in
    match List.length lines with
    | 0 | 1 -> ()
    | n ->
      let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        { pos with pos_bol = String.length & fst & List.last lines;
                   pos_lnum = pos.pos_lnum + (n - 1)
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
  Location.create (start_pos lexbuf) (end_pos lexbuf)

let to_word lexbuf =
  Locating.create (to_loc lexbuf) (lexeme lexbuf)

let strlit_to_word lexbuf read =
  Locating.create
    (Location.create (start_pos lexbuf) (end_pos lexbuf))
    (read (Buffer.create 17) lexbuf)

}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let hex = '0' ['x' 'X']
let digit = ['0'-'9']
let body = (digit|lower|upper|['_' '\''])*
let ident = lower body
let uident = upper body
let octdigit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit+ '.' digit+ exp?
let white = [' ' '\t']+
let nl = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let hexstr = hexdigit hexdigit
let octstr = digit? digit? digit?
let escape = '\\' ['\'' '"' '\\' 'b' 'd' 'e' 'f' 'n' 'r' 's' 't' 'v']
let dqstrchr = escape | [^ '"' '\\' '\r' '\n']
let sqstrchr = escape | [^ '\'' '\\' '\r' '\n']
let octstr = octdigit octdigit? octdigit?
let hexstr = hexdigit hexdigit | '{' hexdigit+ '}'
let ctrlchr = ['a'-'z' 'A'-'Z']
let atom = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let blank = [' ' '\t']*
let space = blank | nl
let dirname = [^' ' '\t' '\r' '\n']+
let comment = [^ '\r' '\n']*


rule token = parse
| blank
    { token lexbuf }
| nl
    { next_line lexbuf; NL (to_loc lexbuf) }
| "#" comment nl
    { next_line lexbuf;
      token lexbuf }
| "#" comment eof
    { EOF (to_loc lexbuf) }
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
    { BOOL (Locating.create (to_loc lexbuf) true) }
| "false"
    { BOOL (Locating.create (to_loc lexbuf) false) }
| "not"
    { NOT (to_loc lexbuf) }
| (digit+ as b) 'r' (['0'-'9' 'a'-'z' 'A'-'Z']+ as v)
    { let b' = int_of_string b in
      if not (2 <= b' && b' <= 36) then
        raise (Error (to_loc lexbuf, "base must be in range 2..36"))
      else
        INT (Locating.create (to_loc lexbuf) (b', v))
    }
| digit+
    { INT (Locating.create (to_loc lexbuf) (10, lexeme lexbuf)) }
| float as s
    { FLOAT (Locating.create (to_loc lexbuf) (float_of_string s)) }
| '-' (* -.����󤷤ˤ��ʤ��Ƥ��ɤ�? ��Ĺ����? *)
    { MINUS (to_loc lexbuf) }
| '+' (* +.����󤷤ˤ��ʤ��Ƥ��ɤ�? ��Ĺ����? *)
    { PLUS (to_loc lexbuf) }
| '/'
    { SLASH (to_loc lexbuf) }
| "-."
    { MINUS_DOT (to_loc lexbuf) }
| "+."
    { PLUS_DOT (to_loc lexbuf) }
| "*."
    { AST_DOT (to_loc lexbuf) }
| "/."
    { SLASH_DOT (to_loc lexbuf) }
| '='
    { EQUAL (to_loc lexbuf) }
| "<>"
    { LESS_GREATER (to_loc lexbuf) }
| "<="
    { LESS_EQUAL (to_loc lexbuf) }
| ">="
    { GREATER_EQUAL (to_loc lexbuf) }
| '<'
    { LESS (to_loc lexbuf) }
| '>'
    { GREATER (to_loc lexbuf) }
| '^'
    { UARROW (to_loc lexbuf) }
| "->"
    { RARROW (to_loc lexbuf) }
| "if"
    { IF (to_loc lexbuf) }
| "then"
    { THEN (to_loc lexbuf) }
| (space* as s) "else"
    { next_line_in_spaces lexbuf s; ELSE (to_loc lexbuf) }
| "in"
    { IN (to_loc lexbuf) }
| "rec"
    { REC (to_loc lexbuf) }
| (space* as s) "def"
    { next_line_in_spaces lexbuf s; DEF (to_loc lexbuf) }
| "external" { EXTERNAL (to_loc lexbuf) }
| "var" { VAR (to_loc lexbuf) }
| "import" { IMPORT (to_loc lexbuf) }
| "as" { AS (to_loc lexbuf) }
| "of" { OF (to_loc lexbuf) }
| "with" { WITH (to_loc lexbuf) }
| "match" { MATCH (to_loc lexbuf) }
| (space* as s) "end"
    { next_line_in_spaces lexbuf s; END (to_loc lexbuf) }
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
    { IDENT (Locating.create (to_loc lexbuf)
        (Id.gentmp (Type.prefix (Type_t.App(Type_t.Unit, []))))) }
| '.'
    { DOT (to_loc lexbuf) }
| '$'
    { DOL (to_loc lexbuf) }
| "<-"
    { LARROW (to_loc lexbuf) }
| ':'
    { COLON (to_loc lexbuf) }
| ';'
    { SEMI (to_loc lexbuf) }
| '\''
    { CHAR (strlit_to_word lexbuf char) }
| '"'
    { STRING (strlit_to_word lexbuf string) }
| '@' atom
    { ATOM (Locating.create (to_loc lexbuf)
              (String.drop 1 & lexeme lexbuf)) }
| '@' '"'
    { ATOM (strlit_to_word lexbuf string) }
| "<<"
    { LESS_LESS (to_loc lexbuf) }
| ">>"
    { GREATER_GREATER (to_loc lexbuf) }
| (space* as s) eof
    { next_line_in_spaces lexbuf s; EOF (to_loc lexbuf) }
| ident
    { IDENT (to_word lexbuf) }
| uident
    { UIDENT (to_word lexbuf) }
| _
    { raise (Error (to_loc lexbuf,
        Printf.sprintf "unknown token '%s'" (lexeme lexbuf))) }

and char buf =
  parse
  | ('\\' octstr as s) '\''
    { s }
  | ('\\' 'x' hexstr as s) '\''
    { s }
  | ('\\' '^' ctrlchr as s) '\''
    { s }
  | (sqstrchr as s) '\''
    { s }
  | '\''
    { raise (Error (to_loc lexbuf, "Empty character")) }
  | [^ '\r' '\n']+ '\''
    { raise (Error (to_loc lexbuf, "Character must be a letter")) }
  | _ { raise (Error (to_loc lexbuf, "Illegal character: " ^ lexeme lexbuf)) }
  | eof { raise (Error (to_loc lexbuf, "Character is not terminated")) }

and string buf =
  parse
  | '"'
    { Buffer.contents buf }
  | nl as s
    { Buffer.add_string buf s;
      next_line lexbuf;
      string buf lexbuf }
  | '\\' octstr
    { Buffer.add_string buf (lexeme lexbuf); string buf lexbuf }
  | '\\' 'x' hexstr
    { Buffer.add_string buf (lexeme lexbuf); string buf lexbuf }
  | '\\' '^' ctrlchr
    { Buffer.add_string buf (lexeme lexbuf); string buf lexbuf }
  | dqstrchr+ as s
    { Buffer.add_string buf s; string buf lexbuf }
  | _ { raise (Error (to_loc lexbuf, "Illegal string character: " ^ lexeme lexbuf)) }
  | eof { raise (Error (to_loc lexbuf, "String is not terminated")) }
