open Base

let bprint_text_of_loc oc fpath loc indent =
  let open Printf in
  let open Location in
  let rec back ic =
    match pos_in ic with
    | 0 -> ()
    | orig ->
      match input_char ic with
      | '\r' | '\n' -> ()
      | c ->
        seek_in ic (orig - 1);
        back ic
  in
  with_ic (open_in fpath)
    (fun ic ->
       seek_in ic (start_offset loc);
       back ic;
       let text = input_line ic in
       bprintf oc "%s%s\n" (String.make indent ' ') text;
       bprintf oc "%s%s\n" (String.make (indent + start_col loc) ' ')
         (String.make (end_offset loc - start_offset loc) '^');
       ())

let print_error fpath loc msg =
  let start_line = Location.start_line loc + 1 in
  let end_line = Location.end_line loc + 1 in
  let start_col = Location.start_col loc + 1 in
  let end_col = Location.end_col loc + 1 in
  if start_line = end_line then begin
    if start_col = end_col then
      Printf.printf "File \"%s\", line %d, characters %d:\n"
        fpath start_line start_col
    else
      Printf.printf "File \"%s\", line %d, characters %d-%d:\n"
        fpath start_line start_col end_col
    end
  else
    Printf.printf "File \"%s\", between line and characters %d:%d-%d:%d:\n"
      fpath start_line start_col end_line end_col;
  Log.error "%s\n" msg;
  exit 1

let bprint_type_error oc fpath t1 t2 =
  let open Printf in
  let print_type oc t =
    let (sl1, sc1, el1, ec1) = Location.values1 t.Location.With.with_ in
    let lc = sprintf "(%d:%d-%d:%d)" sl1 sc1 el1 ec1 in
    let name = Type.to_repr t in
    bprintf oc "    %s    %s"
      (lc ^ (String.make (String.length lc mod 4) ' '))
      (name ^ (String.make (String.length name mod 4) ' '))
  in
  print_type oc t2;
  bprintf oc "    <- actual type\n\n";
  bprint_text_of_loc oc fpath t2.with_ 4;
  bprintf oc "\n";
  print_type oc t1;
  bprintf oc "    <- expected type\n\n";
  bprint_text_of_loc oc fpath t1.with_ 4

let print_exc fpath e =
  let open Printf in
  match e with
  | Lexer.Error (loc, msg) -> print_error fpath loc msg
  | Sig.Error (loc, msg) -> print_error fpath loc msg
  | Ast_t.Syntax_error (loc, None) ->
    print_error fpath loc "Syntax error"
  | Ast_t.Syntax_error (loc, Some msg) ->
    print_error fpath loc ("Syntax error: " ^ msg)
  | Ast_t.Unbound_value_error (loc, x) ->
    (* deprecated *)
    print_error fpath loc ("Unbound value `" ^ x ^ "'")
  | Ast_t.Unbound_module_error (loc, x) ->
    (* deprecated *)
    print_error fpath loc ("Unbound module `" ^ x ^ "'")
  | Naming.Unbound_value_error (loc, x, []) ->
    print_error fpath loc ("Unbound value `" ^ x ^ "'")
  | Naming.Unbound_value_error (loc, x, suggests) ->
    print_error fpath loc
      (sprintf "Unbound value `%s'. Did you mean %s?" x
         (String.concat ", " suggests))
  | Naming.Unbound_constr_error (loc, x, []) ->
    print_error fpath loc ("Unbound constructor `" ^ x ^ "'")
  | Naming.Unbound_constr_error (loc, x, suggests) ->
    print_error fpath loc
      (sprintf "Unbound constructor `%s'. Did you mean %s?" x
         (String.concat ", " suggests))
  | Naming.Unbound_module_error (loc, x, []) ->
    print_error fpath loc ("Unbound module `" ^ x ^ "'")
  | Naming.Unbound_module_error (loc, x, suggests) ->
    print_error fpath loc
      (sprintf "Unbound module `%s'. Did you mean %s?" x
         (String.concat ", " suggests))
  | Typing.Invalid_constr_arguments (loc, x, ex, ac) ->
    print_error fpath loc
      (sprintf "The constructor `%s' expects %d argument(s), but is applied here to %d argument(s)" (Binding.name x) ex ac)
  | Typing.Error (e, t1, t2) ->
    let oc = Buffer.create 16 in
    bprintf oc "Type mismatch: This expression has type `%s', but the expression was expected of type `%s'\n\n"
      (Type.to_repr t2) (Type.to_repr t1);
  bprint_type_error oc fpath t1 t2;
    print_error fpath e.with_ (Buffer.contents oc)
  | Typing.Topdef_error ((x, t), t1, t2) ->
    let oc = Buffer.create 16 in
    bprintf oc "Type mismatch: The argument of function `%s':(%s) should be `%s' instead of `%s'\n\n"
      x (Type.to_repr t) (Type.to_repr t1) (Type.to_repr t2);
    bprint_type_error oc fpath t1 t2;
    print_error fpath t2.with_ (Buffer.contents oc)
  | e -> raise e
