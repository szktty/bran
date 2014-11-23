open Spotlib.Base
open Printf
open Erlang

let gen_var s = "_" ^ s

let gen_arg (x, _) = gen_var x

let iter_with_sep oc sep f es =
  let len = List.length es in
  List.iteri (fun i e ->
      f e;
      if i + 1 < len then
        bprintf oc sep) es

let rec gen_exp oc = function
  | Atom s -> bprintf oc "'%s'" s
  | Int v -> bprintf oc "%d" v
  | String s -> bprintf oc "%s" (Erlang.literal_of_string s)
  | Var x -> bprintf oc "%s" (gen_var x)
  | Tuple es ->
    bprintf oc "{";
    iter_with_sep oc ", " (gen_exp oc) es;
    bprintf oc "}"
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
  | AppDir (Id.L x, es) ->
    bprintf oc "%s(" x;
    iter_with_sep oc ", " (gen_exp oc) es;
    bprintf oc ")"
  | If ptns ->
    bprintf oc "if ";
    iter_with_sep oc "; " (fun (e1, e2) ->
        gen_exp oc e1;
        bprintf oc " -> ";
        gen_exp oc e2) ptns;
    bprintf oc " end"
  | Match (x, pts) ->
    bprintf oc "case %s of " (gen_var x);
    iter_with_sep oc "; " (fun (p, t) ->
        gen_ptn oc p;
        bprintf oc " -> ";
        gen_exp oc t) pts;
    bprintf oc " end"
  | _ -> bprintf oc "ok"

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
  | PtInt v -> bprintf oc "%d" v
  | PtString s -> bprintf oc "\"%s\"" s
  | PtVar x -> bprintf oc "%s" x
  | _ -> assert false (* TODO *)

let gen_def oc = function
  | FunDef { name = (Id.L x, _); args = args; body = body } ->
    bprintf oc "%s(%s) -> " x
      (Xstring.concat_list ", " gen_arg args);
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
  if !Config.escript then begin
    bprintf oc "#!/usr/bin/env escript\n\n";
    bprintf oc "%%%%! -pa erl\n" (* adhoc: standard library path *)
  end;

  bprintf oc "%%%% Note: This code is automatically generated by bran. Do not modify it.\n\n";

  if not !Config.escript then begin
    bprintf oc "-module(%s).\n\n" name;
    gen_export oc defs
  end;

  List.iter (gen_def oc) defs;

  bprintf oc "\n%%%% End.\n"
