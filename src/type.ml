open Base
open Type_t
open Locating

type t = Type_t.t

let cons_id = "Cons"
let nil_id = "Nil"

let counter = ref 0
let newtyvar () = 
  incr counter;
  Printf.sprintf "tyvar_%d" !counter
let newmetavar () = ref None

let rec string_of_t reached t = 
  match t.desc with
  | Var(v) -> "Var(" ^ v ^ ")"
  | Field(tid, t) -> "Field(" ^ (string_of_t reached tid) ^ ", " ^ (string_of_t reached t) ^ ")"
  | App(tycon, ts) -> "App(" ^ (string_of_tycon reached tycon) ^ ", [" ^ (String.concat ", " (List.map (string_of_t reached) ts)) ^ "])"
  | Poly([], t)-> "Poly([], " ^ (string_of_t reached t) ^ ")"
  | Poly(xs, t)-> "Poly(" ^ (String.concat ", " xs) ^ ", " ^ (string_of_t reached t) ^ ")"
  | Meta{ contents = Some(t) } -> "Meta(Some(" ^ (string_of_t reached t) ^ "))"
  | Meta{ contents = None } -> "Meta(None)"
      
and string_of_tycon reached = 
  function
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Int -> "Int"
  | Float -> "Float"
  | Char -> "Char"
  | String -> "String"
  | Atom -> "Atom"
  | Bitstring -> "Bitstring"
  | Binary -> "Binary"
  | Arrow -> "Arrow"
  | List -> "List"
  | Tuple -> "Tuple"
  | Array -> "Array"
  | Module x -> "Module(" ^ x ^ ")"
  | Record(x, fs) -> "Record(" ^ x ^ ", {" ^ (String.concat ", " fs) ^ "})"
  | Variant(x, constrs) when S.mem x reached -> "Variant(" ^ x ^ ")"
  | Variant(x, constrs) -> "Variant(" ^ x ^ ", " ^ (String.concat " | " (List.map (string_of_constr (S.add x reached)) constrs)) ^ ")"
  | TyFun(xs, t) ->
    Printf.sprintf "TyFun([%s], %s)" (String.concat ", " xs) (string_of_t reached t)
  | Instance (xts, t) ->
    Printf.sprintf "Instance([%s], %s)"
      (String.concat ", "
         (List.map (fun (x, t) ->
                      Printf.sprintf "('%s, %s)" x (string_of_t reached t)) xts))
      (string_of_t reached t)
  | NameTycon(x, { contents = None }) when S.mem x reached -> "NameTycon(" ^ x ^ ", None)"
  | NameTycon(x, { contents = None }) -> "NameTycon(" ^ x ^ ", None)"
  | NameTycon(x, { contents = Some(t) }) -> "NameTycon(" ^ x ^ ", Some(" ^ (string_of_tycon reached t) ^ "))"

and string_of_constr reached = 
  function
  | (x, []) -> x
  | (x, ts) -> x ^ " of " ^ (String.concat " * " (List.map (string_of_t reached) ts))

let string_of_t = string_of_t S.empty
let string_of_tycon = string_of_tycon S.empty
let string_of_constr = string_of_constr S.empty
      
let rec prefix t =
  match t.desc with
  | Var _ -> "p" 
  | Field(_, t) -> prefix t
  | App(tycon, _) -> prefix_of_tycon tycon
  | Poly(_, t) -> prefix t
  | _ -> Log.debug "t = %s\n" (string_of_t t); assert false
      
and prefix_of_tycon = 
  function
  | Unit -> "u"
  | Bool -> "b"
  | Int -> "n"
  | Float -> "d"
  | Char -> "c"
  | String -> "s"
  | Atom -> "a"
  | Bitstring -> "bit"
  | Binary-> "bin"
  | Arrow -> "pfn"
  | List -> "l"
  | Tuple -> "t"
  | Array -> "y"
  | Module _ -> "m"
  | Record _ -> "st"
  | Variant _ -> "v"
  | TyFun(_, t) -> prefix t
  | Instance(_, t) -> prefix t
  | NameTycon(x, _) -> x
      
let rec repr_of t =
  match t.desc with
  | Var x -> "'" ^ x
  | Field(_, t) -> repr_of t
  | App(Unit, []) -> "unit"
  | App(Bool, []) -> "bool"
  | App(Int, []) -> "int"
  | App(Float, []) -> "float"
  | App(Atom, []) -> "atom"
  | App(Char, []) -> "char"
  | App(String, []) -> "string"
  | App(Bitstring, []) -> "bitstring"
  | App(Binary, []) -> "binary"
  | App(Arrow, xs) -> String.concat " -> " (List.map repr_of xs)
  | App(List, _) -> "list"
  | App(Tuple, xs) -> "(" ^ (String.concat " * " (List.map repr_of xs)) ^ ")"
  | App(Module x, []) -> "module type " ^ x
  | App(Record(_, xs), ys) -> 
    Printf.sprintf "{ %s }"
      (String.concat_map ", " 
         (fun (x, y) -> x ^ " : " ^ (repr_of y)) (List.combine xs ys))
  | App(Variant(x, _), []) -> x
  | Poly(xs, t) -> repr_of t      
  | App(TyFun([], t), []) -> repr_of t
  | App (Instance ([(_, t1)], t2), _) ->
    Printf.sprintf "%s %s" (repr_of t1) (repr_of t2)
  | App (Instance (xts, t), _) ->
    Printf.sprintf "(%s) %s"
      (String.concat_map ", " (fun (_, t) -> repr_of t) xts)
      (repr_of t)
  | App(NameTycon(x, _), []) -> x
  | App(NameTycon(x, _), [t]) ->
    Printf.sprintf "%s %s" (repr_of t) x
  | App(NameTycon(x, _), ts) ->
    Printf.sprintf "(%s) %s" (String.concat ", " (List.map repr_of ts)) x
  | Meta { contents = None } -> "[?]"
  | Meta { contents = Some t } -> repr_of t
  | _ -> Printf.eprintf "%s : not implemented yet." (string_of_t t); assert false

and repr_of_tycon = function
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | TyFun(_, t) -> repr_of t
  | Variant (_, xts) ->
    String.concat_map " | "
      (fun (x, ts) ->
         match ts with
         | [] -> x
         | _ -> x ^ " of " ^ String.concat_map " * " repr_of ts) xts
  | t -> Printf.eprintf "%s : not implemented yet\n" (string_of_tycon t); assert false

let to_string = string_of_t
let to_repr = repr_of

(* 等値判定。型推論後のみ使用可能。*)
let rec equal t1 t2 = 
  match t1.desc, t2.desc with
  | App(Unit, xs), App(Unit, ys) 
  | App(Bool, xs), App(Bool, ys) 
  | App(Int, xs), App(Int, ys) 
  | App(Arrow, xs), App(Arrow, ys) 
  | App(Tuple, xs), App(Tuple, ys) when List.length xs = List.length ys -> List.for_all2 equal xs ys
  | App(Record(x, _), xs), App(Record(y, _), ys) 
  | App(Variant(x, _), xs), App(Variant(y, _), ys) when List.length xs = List.length ys -> x = y && List.for_all2 equal xs ys
  | App(TyFun(xs, u), ys), t2 -> assert false (* inpossible after Typing.f *)
  | Poly([], u1), _ -> equal u1 t2
  | _, Poly([], u2) -> equal t1 u2
  | Poly(xs, u1), Poly(ys, u2) -> xs = ys && equal u1 u2
  | Var(x), Var(y) -> true
  | Field(_, x), Field(_, y) -> equal x y
  | Meta{ contents = Some(t1') }, _ -> equal t1' t2
  | Meta(x), Meta{ contents = Some(t2') } -> equal t1 t2'
  | Meta(x), Meta(y) when phys_equal x y -> true
  | Meta(x), t2 -> assert false (* inpossible after Typing.f *)
  | _, Meta(y) -> equal t2 t1
  | _, _ -> false
      
let app loc tycon args = create loc & App (tycon, args)
let void_app loc tycon = app loc tycon []
let app_unit loc = create loc (App (Unit, []))

module Tycon = struct

  type t = Type_t.tycon

  let to_string = string_of_tycon
  let to_repr = repr_of_tycon

  (* 型環境 venv に追加する識別子と型のリスト *)
  let vars t =
    Log.debug "# Types.vars %s\n" (string_of_tycon t);
    match t with
    | TyFun (xs, ({ desc = App(Variant(x, constrs), _) } as t)) -> 
      List.map 
        (function
          | y, [] -> y, create t.loc (Poly(xs, t))
          | y, ts -> y, create t.loc (Poly(xs, create t.loc (App(Arrow, ts @ [t])))))
        constrs
    | _ -> []

  (* 型環境 tenv に追加する識別子と型のリスト *)
  let types t =
    Log.debug "# Types.types %s\n" (string_of_tycon t);
    match t with
    | TyFun(xs, ({ desc = App(Record(x, fs), ys) } as t)) ->
      (List.combine fs (List.map (fun y ->
           create t.loc (Poly (xs, create t.loc (Field(t, y)))))  ys))
    | _ -> []

end

module Constr = struct

  type t = Type_t.constr

  let to_string = string_of_constr

end
