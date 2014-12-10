{
open Lexing
open Parser
open Base

exception Error of Location.t * string

let curr = ref { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  curr := { pos with pos_bol = lexbuf.lex_curr_pos;
                     pos_lnum = pos.pos_lnum + 1 }

let next_line_in_spaces lexbuf s =
  let (_, pos) =
    List.fold_left
      (fun (cnum, pos) (s, nl) ->
         let cnum' = cnum + String.length s + String.length nl in
         (cnum',
          begin match nl with
            | "" -> pos
            | _ -> { pos with pos_bol = cnum';
                              pos_lnum = pos.pos_lnum + 1 }
          end))
      (lexbuf.lex_curr_pos - String.length s, !curr) & String.lines s
  in
  curr := pos

let start_pos lexbuf =
  let pos = lexeme_start_p lexbuf in
  Position.of_lexing_pos
    { pos with pos_lnum = !curr.pos_lnum;
               pos_bol = pos.pos_cnum - !curr.pos_bol;
               pos_cnum = pos.pos_cnum }

let end_pos lexbuf =
  let pos = lexeme_end_p lexbuf in
  Position.of_lexing_pos
    { pos with pos_lnum = !curr.pos_lnum;
               pos_bol = pos.pos_cnum - !curr.pos_bol - 1;
               pos_cnum = pos.pos_cnum }

let to_loc lexbuf =
    let l = Location.create (start_pos lexbuf) (end_pos lexbuf) in
    (* Printf.printf "# Lexer: %s: \"%s\"\n" (Location.to_string l) (lexeme lexbuf); *)
    l

let to_word lexbuf =
  Locating.create (to_loc lexbuf) (lexeme lexbuf)

let strlit_to_word lexbuf read =
  Locating.create
    (Location.create (start_pos lexbuf) (end_pos lexbuf))
    (read (Buffer.create 17) lexbuf)

let with_word lexbuf s =
  Locating.create (to_loc lexbuf) s

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
let sqchr = escape | [^ '\'' '\\' '\r' '\n']
let octstr = octdigit octdigit? octdigit?
let hexstr = hexdigit hexdigit | '{' hexdigit+ '}'
let ctrlchr = ['a'-'z' 'A'-'Z']
let atom = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let blank = [' ' '\t']
let space = blank | nl
let dirname = [^' ' '\t' '\r' '\n']+
let comment = [^ '\r' '\n']*


rule token = parse
| blank+
    { token lexbuf }
| nl+ as s
    { next_line_in_spaces lexbuf s; NL (to_loc lexbuf) }
| (nl* blank* '#' comment (nl blank* '#' comment)*) as s
    { next_line_in_spaces lexbuf s; token lexbuf }
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
| "assert"
    { ASSERT (to_loc lexbuf) }
| "as"
    { AS (to_loc lexbuf) }
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
| '-'
    { MINUS (to_loc lexbuf) }
| '+'
    { PLUS (to_loc lexbuf) }
| '*'
    { AST (to_loc lexbuf) }
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
| "else"
    { ELSE (to_loc lexbuf) }
| "in"
    { IN (to_loc lexbuf) }
| "rec"
    { REC (to_loc lexbuf) }
| "def"
    { if (start_pos lexbuf).col = 0 then
        TOPDEF (to_loc lexbuf)
      else
        DEF (to_loc lexbuf)
    }
| "external" { EXTERNAL (to_loc lexbuf) }
| "var"
    { if (start_pos lexbuf).col = 0 then
        TOPVAR (to_loc lexbuf)
      else
        VAR (to_loc lexbuf)
    }
| "of" { OF (to_loc lexbuf) }
| "with"
    { WITH (to_loc lexbuf) }
| "match" { MATCH (to_loc lexbuf) }
| "end"
    { END (to_loc lexbuf) }
| "exception"
    { EXCEPTION (to_loc lexbuf) }
| "do"
    { DO (to_loc lexbuf) }
| "for" { FOR (to_loc lexbuf) }
| "fun" { FUN (to_loc lexbuf) }
| "raise" { RAISE (to_loc lexbuf) }
| "to" { TO (to_loc lexbuf) }
| "try" { TRY (to_loc lexbuf) }
| "type" { TYPE (to_loc lexbuf) }
| "mod" { MOD (to_loc lexbuf) }
| "perform" { PERFORM (to_loc lexbuf) }
| "return" { RETURN (to_loc lexbuf) }
| "receive" { RECEIVE (to_loc lexbuf) }
| "and" { AND (to_loc lexbuf) }
| ','
    { COMMA (to_loc lexbuf) }
| '_'
    { let loc = to_loc lexbuf in
      IDENT (Locating.create loc
        (Id.gentmp (Type.prefix
                      (Locating.create loc (Type_t.App(Type_t.Unit, [])))))) }
| '.'
    { DOT (to_loc lexbuf) }
| '$'
    { DOL (to_loc lexbuf) }
| "<-"
    { LARROW (to_loc lexbuf) }
| ':'
    { COLON (to_loc lexbuf) }
| "::"
    { CONS (to_loc lexbuf) }
| ":="
    { ASSIGN (to_loc lexbuf) }
| ';'
    { SEMI (to_loc lexbuf) }
| '!'
    { EXCL (to_loc lexbuf) }
| '|'
    { PIPE (to_loc lexbuf) }
| ''' (sqchr as s) '''
    { CHAR (with_word lexbuf s) }
| ''' ('\\' octstr as s) '''
    { CHAR (with_word lexbuf s) }
| ''' ('\\' 'x' hexstr as s) '''
    { CHAR (with_word lexbuf s) }
| ''' ('\\' '^' ctrlchr as s) '''
    { CHAR (with_word lexbuf s) }
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
| eof
    { EOF (to_loc lexbuf) }
| ident
    { IDENT (to_word lexbuf) }
| uident
    { UIDENT (to_word lexbuf) }
| ''' (ident as s)
    { QIDENT (Locating.create (to_loc lexbuf) s) }
| _
    { raise (Error (to_loc lexbuf,
        Printf.sprintf "unknown token '%s'" (lexeme lexbuf))) }

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
