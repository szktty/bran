open Spotlib
open Asm
open Base
open Printf

  (*
external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"
   *)
(*
let gethi _ = Int32.zero
let getlo _ = Int32.zero
 *)

let stackset = ref S.empty (* すでにSaveされた変数の集合 (caml2html: emit_stackset) *)
let stackmap = ref [] (* Saveされた変数の、スタックにおける位置 (caml2html: emit_stackmap) *)
let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let savef x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    (let pad =
      if List.length !stackmap mod 2 = 0 then [] else [Id.gentmp Type.Int] in
    stackmap := !stackmap @ pad @ [x; x])
let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs) in
  loc !stackmap
let offset x = 4 * List.hd (locate x)
let stacksize () = align (List.length !stackmap * 4)

let pp_id_or_imm = function
  | V(x) -> x
  | C(i) -> "$" ^ string_of_int i

(* 関数呼び出しのために引数を並べ替える(register shuffling) (caml2html: emit_shuffle) *)
let rec shuffle sw xys =
  (* remove identical moves *)
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
  (* find acyclic moves *)
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
  | [], [] -> []
  | (x, y) :: xys, [] -> (* no acyclic moves; resolve a cyclic move *)
      (y, sw) :: (x, y) :: shuffle sw (List.map
					 (function
					   | (y', z) when y = y' -> (sw, z)
					   | yz -> yz)
					 xys)
  | xys, acyc -> acyc @ shuffle sw xys

let genvar x =
  match String.get x 0 with
  | 'P' | 'T' | '_' -> x
  | _ -> "_V" ^ x

type dest = Tail | NonTail of Id.t (* 末尾かどうかを表すデータ型 (caml2html: emit_dest) *)
let rec g oc = function (* 命令列のアセンブリ生成 (caml2html: emit_g) *)
  | dest, Ans(exp) -> g' oc (dest, exp)
  | dest, Let((x, t), exp, e) ->
      g' oc (NonTail(x), exp);
      g oc (dest, e)
and g' oc e = (* 各命令のアセンブリ生成 (caml2html: emit_gprime) *)
  (* 末尾でなかったら計算結果をdestにセット (caml2html: emit_nontail) *)
  match e with
  | NonTail(_), Nop -> assert false
  | NonTail(x), Set(i) ->
    bprintf oc "\t%s = %d,\n" x i
  | NonTail(x), SetL(Id.L(y)) ->
    bprintf oc "\t%s = ?%s,\n" x y
  | NonTail(x), SetL(Id.M(y)) ->
    bprintf oc "\t%s = fun %s,\n" x y (* TODO: arity? *)
  | NonTail(x), SetB(b) ->
    bprintf oc "\t%s = %s,\n" x (if b then "true" else "false")
  | NonTail(x), Tuple ys ->
    bprintf oc "\t%s = {%s},\n" x (String.concat ", " (List.map genvar ys))
  | NonTail(x), List ys ->
    bprintf oc "\t%s = [%s],\n" x (String.concat ", " (List.map genvar ys))
  | NonTail(x), Mov(y) ->
      if x <> y then bprintf oc "\t%s = %s,\n" x y
    | NonTail(x), Neg(y) ->
        if x <> y then bprintf oc "\tmovl\t%s, %s\n" y x;
      bprintf oc "\tnegl\t%s\n" x
  | NonTail(x), Add(y, z') ->
    if V(x) = z' then
      bprintf oc "\t%s + %s\n" y x
    else if x <> y then
      bprintf oc "\t%s = %s + %s,\n" (genvar x) (genvar y)
        (genvar (pp_id_or_imm z'))
  | NonTail(x), Sub(y, z') ->
    if V(x) = z' then
      bprintf oc "\t%s - %s,\n" x (genvar y)
    else if x <> y then
      bprintf oc "\t%s = %s - %s,\n" x (genvar y) (pp_id_or_imm z')
  | NonTail(x), Ld(y, V(z), i) -> bprintf oc "\tmovl\t(%s,%s,%d), %s\n" y z i x
  | NonTail(x), Ld(y, C(j), i) ->
    (*bprintf oc "\tmovl\t%d(%s), %s\n" (j * i) y x*)
    ()
  | NonTail(_), St(x, y, V(z), i) -> bprintf oc "\tmovl\t%s, (%s,%s,%d)\n" x y z i
  | NonTail(_), St(x, y, C(j), i) -> bprintf oc "\tmovl\t%s, %d(%s)\n" x (j * i) y
  | NonTail(x), FMovD(y) ->
    if x <> y then bprintf oc "\t%s = %s,\n" x y
  | NonTail(x), FNegD(y) ->
      if x <> y then bprintf oc "\tmovsd\t%s, %s\n" y x;
      bprintf oc "\txorpd\tmin_caml_fnegd, %s\n" x
  | NonTail(x), FAddD(y, z) ->
    if x = z then (* TODO *)
      bprintf oc "\taddsd\t%s, %s\n" y x
    else if x <> y then
      bprintf oc "\t%s = %s + %s,\n" (genvar x) (genvar y) (genvar z)
  | NonTail(x), FSubD(y, z) ->
      if x = z then (* [XXX] ugly *)
	let ss = stacksize () in
	bprintf oc "\tmovsd\t%s, %d(%s)\n" z ss reg_sp;
	if x <> y then bprintf oc "\tmovsd\t%s, %s\n" y x;
	bprintf oc "\tsubsd\t%d(%s), %s\n" ss reg_sp x
      else
	(if x <> y then bprintf oc "\t%s = %s - %s,\n" (genvar x) (genvar y) (genvar z))
  | NonTail(x), FMulD(y, z) ->
      if x = z then
        bprintf oc "\tmulsd\t%s, %s\n" y x
      else if x <> y then
        bprintf oc "\t%s = %s * %s,\n" (genvar x) (genvar y) (genvar z)
  | NonTail(x), FDivD(y, z) ->
      if x = z then (* [XXX] ugly *)
	let ss = stacksize () in
	bprintf oc "\tmovsd\t%s, %d(%s)\n" z ss reg_sp;
	if x <> y then bprintf oc "\tmovsd\t%s, %s\n" y x;
	bprintf oc "\tdivsd\t%d(%s), %s\n" ss reg_sp x
    else if x <> y then
      bprintf oc "\t%s = %s / %s,\n" (genvar x) (genvar y) (genvar z)
  | NonTail(x), SConcat(y, z) ->
    if x = z then
      bprintf oc "\t%s ++ %s,\n" y x
    else if x <> y then
      bprintf oc "\t%s = %s ++ %s,\n" x (genvar y) (genvar z)
  | NonTail(x), LdDF(y, V(z), _i) ->
    bprintf oc "\t%s = %s,\n" x y
  | NonTail(x), LdDF(y, C(j), _i) ->
    bprintf oc "\t%s = %s,\n" x y
  | NonTail(_), StDF(x, y, V(z), i) -> bprintf oc "\tmovsd\t%s, (%s,%s,%d)\n" x y z i
  | NonTail(_), StDF(x, y, C(j), i) -> bprintf oc "\tmovsd\t%s, %d(%s)\n" x (j * i) y
  | NonTail(_), Comment(s) -> bprintf oc "\t%%%% %s\n" s
  (* 退避の仮想命令の実装 (caml2html: emit_save) *)
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
      bprintf oc "\tmovl\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
      savef y;
      bprintf oc "\tmovsd\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ()
  (* 復帰の仮想命令の実装 (caml2html: emit_restore) *)
  | NonTail(x), Restore(y) when List.mem x allregs ->
      bprintf oc "\tmovl\t%d(%s), %s\n" (offset y) reg_sp x
  | NonTail(x), Restore(y) ->
      assert (List.mem x allfregs);
      bprintf oc "\tmovsd\t%d(%s), %s\n" (offset y) reg_sp x
  (* 末尾だったら計算結果を第一レジスタにセットしてret (caml2html: emit_tailret) *)
  | Tail, (Nop | St _ | StDF _ | Comment _ | Save _ as exp) ->
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      bprintf oc "\tret\n";
  | Tail, (Set _ | SetB _ | SetL _ | Tuple _ | List _ | Mov _ | Neg _ | Add _ | Sub _ | Ld _ as exp) ->
    (* TODO: type *)
    let ret = Id.gentmp Type.Int in
    g' oc (NonTail(ret), exp);
    bprintf oc "\t%s %% Tail, base op\n" ret;
  | Tail, (FMovD _ | FNegD _ | FAddD _ | FSubD _ | FMulD _ | FDivD _ | SConcat _ | LdDF _  as exp) ->
    let ret = Id.gentmp Type.Float in
    g' oc (NonTail(ret), exp);
    bprintf oc "\t%s %% Tail, float op\n" ret;
  | Tail, (Restore(x) as exp) ->
      (match locate x with
      | [i] -> g' oc (NonTail(regs.(0)), exp)
      | [i; j] when i + 1 = j -> g' oc (NonTail(fregs.(0)), exp)
      | _ -> assert false);
      bprintf oc "\tret\n";
  | Tail, IfEq(x, y', e1, e2) ->
      bprintf oc "\tif %s =/= %s ->\n" (genvar x) (pp_id_or_imm y');
      g'_tail_if oc e1 e2 "je" "jne"
  | Tail, IfLE(x, y', e1, e2) ->
      bprintf oc "\tif %s =< %s ->\n" (genvar x) (pp_id_or_imm y');
      g'_tail_if oc e1 e2 "jle" "jg"
  | Tail, IfGE(x, y', e1, e2) ->
      bprintf oc "\tif %s >= %s ->\n" (genvar x) (pp_id_or_imm y');
      g'_tail_if oc e1 e2 "jge" "jl"
  | Tail, IfFEq(x, y, e1, e2) ->
      bprintf oc "\tif %s =/= %s ->\n" (genvar x) y;
      g'_tail_if oc e1 e2 "je" "jne"
  | Tail, IfFLE(x, y, e1, e2) ->
      bprintf oc "\tif %s =< %s ->\n" (genvar x) y;
      g'_tail_if oc e1 e2 "jbe" "ja"
  | NonTail(z), IfEq(x, y', e1, e2) ->
      bprintf oc "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "je" "jne"
  | NonTail(z), IfLE(x, y', e1, e2) ->
      bprintf oc "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "jle" "jg"
  | NonTail(z), IfGE(x, y', e1, e2) ->
      bprintf oc "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "jge" "jl"
  | NonTail(z), IfFEq(x, y, e1, e2) ->
      bprintf oc "\tcomisd\t%s, %s\n" y x;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "je" "jne"
  | NonTail(z), IfFLE(x, y, e1, e2) ->
      bprintf oc "\tcomisd\t%s, %s\n" y x;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "jbe" "ja"
  (* 関数呼び出しの仮想命令の実装 (caml2html: emit_call) *)
  | Tail, CallCls(x, ys, zs) -> (* 末尾呼び出し (caml2html: emit_tailcall) *)
      (*g'_args oc [(x, reg_cl)] ys zs;*)
      bprintf oc "\t%s(%s) %% Tail, CallCls\n" x (String.concat ", " (ys @ zs))
  | Tail, CallDir(Id.L(x), ys, zs) -> (* 末尾呼び出し *)
      (*g'_args oc [] ys zs;*)
      bprintf oc "\t%s(%s) %% Tail, CallDir\n" x (String.concat ", " (ys @ zs))
  | Tail, CallDir(Id.M(x), ys, zs) -> (* 末尾呼び出し *)
      (*g'_args oc [] ys zs;*)
      bprintf oc "\t%s(%s) %% Tail, CallDir\n" x (String.concat ", " (ys @ zs))
  | NonTail(a), CallCls(x, ys, zs) ->
      (*g'_args oc [(x, reg_cl)] ys zs;*)
      let ss = stacksize () in
      if ss > 0 then bprintf oc "\taddl\t$%d, %s\n" ss reg_sp;
      bprintf oc "\t%s(%s), %% NonTail, CallCls\n" x (String.concat ", " (ys @ zs));
      if ss > 0 then bprintf oc "\tsubl\t$%d, %s\n" ss reg_sp;
      if List.mem a allregs && a <> regs.(0) then
        bprintf oc "\tmovl\t%s, %s\n" regs.(0) a
      else if List.mem a allfregs && a <> fregs.(0) then
        bprintf oc "\tmovsd\t%s, %s\n" fregs.(0) a
  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
      (*
      g'_args oc [] ys zs;
      let ss = stacksize () in
      if ss > 0 then bprintf oc "\taddl\t$%d, %s\n" ss reg_sp;
      bprintf oc "\t%s(), %% NonTail, CallDir\n" x;
      if ss > 0 then bprintf oc "\tsubl\t$%d, %s\n" ss reg_sp;
      if List.mem a allregs && a <> regs.(0) then
        bprintf oc "\tmovl\t%s, %s\n" regs.(0) a
      else if List.mem a allfregs && a <> fregs.(0) then
        bprintf oc "\tmovsd\t%s, %s\n" fregs.(0) a
       *)
    bprintf oc "\t%s = %s(%s), %% NonTail, CallDir\n"
        a x (String.concat ", " (ys @ zs))
  | NonTail(a), CallDir(Id.M(x), ys, zs) ->
    bprintf oc "\t%s = %s(%s), %% NonTail, CallDir\n"
        a x (String.concat ", " (ys @ zs))
and g'_tail_if oc e1 e2 b bn =
  (*let b_else = Id.genid (b ^ "_else") in*)
  let stackset_back = !stackset in
  g oc (Tail, e1);
  bprintf oc "\t; true ->\n";
  stackset := stackset_back;
  g oc (Tail, e2);
  bprintf oc "\tend\n"
and g'_non_tail_if oc dest e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
  bprintf oc "\t%s\t%s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (dest, e1);
  let stackset1 = !stackset in
  bprintf oc "\tjmp\t%s\n" b_cont;
  bprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (dest, e2);
  bprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
and _g'_args oc x_reg_cl ys zs =
  assert (List.length ys <= Array.length regs - List.length x_reg_cl);
  assert (List.length zs <= Array.length fregs);
  let sw = sprintf "%d(%s)" (stacksize ()) reg_sp in
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys in
  List.iter
    (fun (y, r) -> bprintf oc "\tmovl\t%s, %s\n" y r)
    (shuffle sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs in
  List.iter
    (fun (z, fr) -> bprintf oc "\tmovsd\t%s, %s\n" z fr)
    (shuffle sw zfrs)

let h oc { name = name; ptn = ptn; args = args; fargs = _; body = e; ret = _ } =
  (* TODO: 引数 *)
  let x = match name with
    | Id.L(x) -> x
    | _ -> assert false
  in
  let genptn ptn =
    let rec f = function
    | FunArg.Var (x, _) -> genvar x
    | FunArg.Tuple (_, es) ->
      sprintf "{%s}" (String.concat ", " & List.map f es)
    in
    f ptn
  in
  bprintf oc "%s(%s) ->\n" x (String.concat ", " & List.map genptn ptn);
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e);
  bprintf oc "\t.\n\n"

let find_fundef fundefs x =
  List.find (fun fundef ->
    match fundef.name with
    | L lx -> x = lx
    | M _ -> false) fundefs

let f name oc (Prog(data, sdata, fundefs, e)) =
  let open Printf in
  if !Config.verbose then
    printf "# generating assembly...\n";

  if !Config.escript then begin
    bprintf oc "#!/usr/bin/env escript\n\n";
    bprintf oc "%%%%! -pa erl\n" (* adhoc: standard library path *)
  end;

  bprintf oc "%%%% Note: This code is automatically generated by bran. Do not modify it.\n\n";

  if not !Config.escript then begin
    bprintf oc "-module(%s).\n\n" name;
    bprintf oc "-export([";
    bprintf oc "%s" & String.concat ", " & List.map
      (fun (x, x') ->
         let def = find_fundef fundefs x' in
         sprintf "%s/%d" x (List.length def.ptn))
      (Context.top_funs ());
    bprintf oc "]).\n\n";
  end;

  List.iter (fun (id, d) ->
               match id with
               | Id.M _ -> ()
               | Id.L(x) -> bprintf oc "-define(%s, %s).\n" x
                              (Erlang.literal_of_float d)) data;
  bprintf oc "\n";
  List.iter (fun (id, d) ->
               match id with
               | Id.M _ -> ()
               | Id.L(x) -> bprintf oc "-define(%s, \"%s\").\n" x d) sdata;
  bprintf oc "\n";

  List.iter (fun fundef -> h oc fundef) fundefs;

  bprintf oc "\n%%%% export functions\n";
  let tmps count =
    let rec f accu = function
    | 0 -> List.rev accu
    | v -> sprintf "V%d" v :: f accu (v - 1)
    in
    f [] count
  in
  List.iter (fun (x, x') ->
         let def = find_fundef fundefs x' in
         let args = String.concat ", " (tmps (List.length def.ptn)) in
         bprintf oc "%s(%s) -> %s(%s).\n" x args x' args)
      (Context.top_funs ());

  bprintf oc "\n%%%% End.\n"
