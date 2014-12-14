open Base
open Printf
open Erlang_t

let mod_name = ref ""

let gen_var s = "_" ^ s

let gen_arg (x, _) = gen_var x

let inject_sep oc sep f es =
  List.inject (fun () -> bprintf oc sep) f es

let rec gen_exp oc = function
  | Atom s -> bprintf oc "'%s'" s
  | Int (b, v) -> bprintf oc "%d#%s" b v
  | Float v -> bprintf oc "%f" v
  | Char s -> bprintf oc "$%s" s
  | String s -> bprintf oc "%s" (Erlang.literal_of_string s)
  | Bitstring es ->
    begin
      let open Bitstring in
      bprintf oc "<<";
      bprintf oc "%s" & String.concat_map ", "
        (fun e ->
          begin match e.Bits.value with
            | Bits.Int v -> sprintf "%d" v
            | Bits.Float v -> sprintf "%f" v
            | Bits.String v -> sprintf "\"%s\"" v
            | Bits.Var v -> sprintf "%s" (gen_var v)
          end ^
          begin match e.Bits.size with
            | None -> ""
            | Some v -> ":" ^ string_of_int v
          end ^ "/" ^
          begin match e.Bits.typ with
            | `Int -> "integer"
            | `Float -> "float"
            | `Binary -> "binary"
            | `Bitstring -> "bitstring"
            | `UTF8 -> "utf8"
            | `UTF16 -> "utf16"
            | `UTF32 -> "utf32"
          end ^
          begin match e.Bits.sign with
            | None -> ""
            | Some `Unsigned -> "-unsigned"
            | Some `Signed -> "-signed"
          end ^
          begin match e.Bits.endian with
            | None -> ""
            | Some `Big -> "-big"
            | Some `Little -> "-little"
            | Some `Native -> "-native"
          end ^
          begin match e.Bits.unit with
            | None -> ""
            | Some v -> sprintf "-unit:%d" v
          end) es;
      bprintf oc ">>";
    end
  | Var (`Local x) -> bprintf oc "%s" (gen_var x)
  | Var (`Module x) -> bprintf oc "%s()" (Binding.to_erl_fun x)
  | Tuple es ->
    bprintf oc "{";
    inject_sep oc ", " (gen_exp oc) es;
    bprintf oc "}"
  | List es ->
    bprintf oc "[";
    inject_sep oc ", " (gen_exp oc) es;
    bprintf oc "]"
  | Array es ->
    bprintf oc "array:from_list([";
    inject_sep oc ", " (gen_exp oc) es;
    bprintf oc "])"
  | Not e -> gen_prefix_exp oc "not" e
  | And (e1, e2) -> gen_bin_exp oc e1 "and" e2
  | Or (e1, e2) -> gen_bin_exp oc e1 "or" e2
  | Neg e -> gen_prefix_exp oc "-" e
  | Add (e1, e2) -> gen_bin_exp oc e1 "+" e2
  | Sub (e1, e2) -> gen_bin_exp oc e1 "-" e2
  | Mul (e1, e2) -> gen_bin_exp oc e1 "*" e2
  | Div (e1, e2) -> gen_bin_exp oc e1 "/" e2
  | Concat (e1, e2) -> gen_bin_exp oc e1 "++" e2
  | Eq (e1, e2) -> gen_bin_exp oc e1 "=:=" e2
  | LE (e1, e2) -> gen_bin_exp oc e1 "=<" e2
  | AppDir (x, es) ->
    bprintf oc "%s(" (Binding.to_erl_fun x);
    inject_sep oc ", " (gen_exp oc) es;
    bprintf oc ")"
  | If ptns ->
    bprintf oc "if ";
    inject_sep oc "; " (fun (e1, e2) ->
        gen_exp oc e1;
        bprintf oc " -> ";
        gen_exp oc e2) ptns;
    bprintf oc " end"
  | Match (x, pts) ->
    bprintf oc "case %s of " (gen_var x);
    inject_sep oc "; " (fun (p, t) ->
        gen_ptn oc p;
        bprintf oc " -> ";
        gen_exp oc t) pts;
    bprintf oc " end"
  | Let (x, e1, e2) ->
    bprintf oc "%s = " (gen_var x);
    gen_exp oc e1;
    bprintf oc ", ";
    gen_exp oc e2
  | Constr (x, []) ->
    Log.debug "# Constr %s\n" (Binding.to_erl_atom x);
    bprintf oc "%s" (Binding.to_erl_atom x)
  | Constr (x, es) ->
    bprintf oc "{%s, " (Binding.to_erl_atom x);
    inject_sep oc ", " (gen_exp oc) es;
    bprintf oc "}"
  | _ -> assert false

and gen_prefix_exp oc op e =
    bprintf oc "(";
    gen_exp oc e;
    bprintf oc ")"

and gen_bin_exp oc e1 op e2 =
    bprintf oc "(";
    gen_exp oc e1;
    bprintf oc " %s " op;
    gen_exp oc e2;
    bprintf oc ")"

and gen_ptn oc = function
  | PtAtom s -> bprintf oc "'%s'" s
  | PtBool v -> bprintf oc "%s" (string_of_bool v)
  | PtInt (b, v) -> bprintf oc "%d#%s" b v
  | PtString s -> bprintf oc "\"%s\"" s
  | PtVar x -> bprintf oc "%s" (gen_var x)
  | PtTuple ps ->
    bprintf oc "{";
    inject_sep oc ", " (fun p -> gen_ptn oc p) ps;
    bprintf oc "}"
  | _ -> assert false (* TODO *)

let rec gen_type_tycon ~format oc t =
  let open Type_t in
  match t with
  | Unit -> bprintf oc "{}"
  | Bool -> bprintf oc "bool()"
  | Int -> bprintf oc "integer()"
  | Float -> bprintf oc "float()"
  | Char -> bprintf oc "char()"
  | String -> bprintf oc "string()"
  | Atom -> bprintf oc "atom()"
  | Bitstring -> bprintf oc "bitstring()"
  | Binary -> bprintf oc "binary()"
  | Variant (tx, xts) ->
    begin match format with
    | `Spec -> bprintf oc "%s()" tx
    | `Type ->
      inject_sep oc " | "
        (fun (x, ts) ->
           match ts with
           | [] -> bprintf oc "'%s.%s'" !mod_name x
           | _ ->
             bprintf oc "{'%s.%s', " !mod_name x;
             inject_sep oc ", " (gen_type ~format oc) ts;
             bprintf oc "}") xts
    end
  | TyFun (_, t) -> gen_type ~format oc t
  | _ -> assert false

and gen_type ~format oc t =
  let open Type_t in
  match t.desc with
  | Var _ -> bprintf oc "any()"
  | App (Tuple, ts) ->
    bprintf oc "{";
    inject_sep oc ", " (gen_type ~format oc) ts;
    bprintf oc "}"
  | App (List, t :: _) ->
    bprintf oc "[";
    gen_type ~format oc t;
    bprintf oc "]"
  | App (Record (rx, xs), ts) ->
    bprintf oc "{";
    List.inject2
      (fun () -> bprintf oc ", ")
      (fun x t ->
         bprintf oc "%s::" x;
         gen_type ~format oc t) xs ts;
    bprintf oc "}"
  | App (Arrow, ts) ->
    let (ts', t') = match List.rev ts with
      | t' :: ts' -> List.rev ts', t'
      | _ -> assert false
    in
    bprintf oc "(";
    inject_sep oc ", " (gen_type ~format oc) ts';
    bprintf oc ") -> ";
    gen_type ~format oc t'
  | App (Instance ([(_, t)], { desc = App (List, []) }), _) ->
    bprintf oc "[";
    gen_type ~format oc t;
    bprintf oc "]"
  | App (Instance (_, t), _) -> gen_type ~format oc t
  | App (tycon, []) -> gen_type_tycon ~format oc tycon
  | Poly (_, t) -> gen_type ~format oc t
  | _ ->
    Printf.printf "Error: not implemented: %s\n" (Type.to_string t);
    assert false

let gen_def oc = function
  | TypeDef (x, tycon) ->
    begin match tycon with
    | Type_t.TyFun (_, { desc = Type_t.App (Type_t.Record _, _) }) ->
      bprintf oc "-record(%s, " x;
      gen_type_tycon oc tycon ~format:`Type;
      bprintf oc ").\n"
    | _ ->
      bprintf oc "-type %s() :: " x;
      gen_type_tycon oc tycon ~format:`Type;
      bprintf oc ".\n"
    end
  | FunDef { name = (Id.L x, t); args = args; body = body } ->
    if !Config.gen_spec then begin
      bprintf oc "-spec %s" x;
      gen_type oc t ~format:`Spec;
      bprintf oc ".\n"
    end;
    bprintf oc "%s(%s) -> " x
      (String.concat_map ", " gen_arg args);
    gen_exp oc body;
    bprintf oc ".\n"
  | _ -> ()

let gen_export oc defs =
  let sigs =
    List.rev & List.fold_left (fun accu def ->
        match def with
        | FunDef { name = (Id.L x, _); args = args } -> 
          (sprintf "%s/%d" x (List.length args)) :: accu
        | _ -> accu) [] defs
  in
  if List.length sigs > 0 then
    bprintf oc "-export([%s]).\n\n" (String.concat ", " sigs)

let f name oc (Prog defs) =
  mod_name := Utils.module_name name;
  bprintf oc "%%%% Note: This code is automatically generated by bran. Do not modify it.\n\n";
  bprintf oc "-module(%s).\n\n" name;
  gen_export oc defs;
  List.iter (gen_def oc) defs;
  bprintf oc "\n%%%% End.\n"
