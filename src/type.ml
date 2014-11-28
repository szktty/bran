type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Var of tyvar
  | Field of t * t (* レコードの型 * フィールドの型 *)
  | App of tycon * t list
  | Poly of tyvar list * t
  | Meta of t option ref (* 型推論であとで代入するために ref 型になっている *)
and tycon =
  | Unit
  | Bool
  | Int
  | String
  | Atom
  | Arrow
  | Tuple
  | Record of Id.t * Id.t list (* 型名とフィールド識別子のリスト。型名はあとで名前引きやすいようにするため *)
  | Variant of Id.t * constr list (* 最初のId.tは型名。理由は同上 *)
  | TyFun of tyvar list * t
  | NameTycon of Id.t * tycon option ref 
  | Module of Id.t
and tyvar = Id.t
and metavar = Id.t
and constr = Id.t * t list

let counter = ref 0
let newtyvar () = 
  incr counter;
  Printf.sprintf "tyvar_%d" !counter
let newmetavar () = ref None

let rec string_of_t reached = 
  function
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
  | String -> "String"
  | Atom -> "Atom"
  | Arrow -> "Arrow"
  | Tuple -> "Tuple"
  | Module x -> "Module(" ^ x ^ ")"
  | Record(x, fs) -> "Record(" ^ x ^ ", {" ^ (String.concat ", " fs) ^ "})"
  | Variant(x, constrs) when S.mem x reached -> "Variant(" ^ x ^ ")"
  | Variant(x, constrs) -> "Variant(" ^ x ^ ", " ^ (String.concat " | " (List.map (string_of_constr (S.add x reached)) constrs)) ^ ")"
  | TyFun(xs, t) -> "TyFun(" ^ (String.concat ", " xs) ^ ", " ^ (string_of_t reached t) ^ ")"
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
      
let rec prefix = 
  function
  | Var _ -> "p" 
  | Field(_, t) -> prefix t
  | App(tycon, _) -> prefix_of_tycon tycon
  | Poly(_, t) -> prefix t
  | t -> Log.debug "t = %s\n" (string_of_t t); assert false
      
and prefix_of_tycon = 
  function
  | Unit -> "u"
  | Bool -> "b"
  | Int -> "n"
  | String -> "s"
  | Atom -> "a"
  | Arrow -> "pfn"
  | Tuple -> "t"
  | Module _ -> "m"
  | Record _ -> "st"
  | Variant _ -> "v"
  | TyFun(_, t) -> prefix t
  | NameTycon(x, _) -> x
      
let rec ocaml_of = function
  | Var _ -> "'a"
  | Field(_, t) -> ocaml_of t
  | App(Unit, []) -> "unit"
  | App(Bool, []) -> "bool"
  | App(Int, []) -> "int"
  | App(String, []) -> "string"
  | App(Arrow, xs) -> String.concat " -> " (List.map ocaml_of xs)
  | App(Tuple, xs) -> "(" ^ (String.concat " * " (List.map ocaml_of xs)) ^ ")"
  | App(Module x, []) -> "module type " ^ x
  | App(Record(_, xs), ys) -> 
      "{" ^ (String.concat ";" 
               (List.map (fun (x, y) -> x ^ " = " ^ (ocaml_of y)) (List.combine xs ys))) ^ "}"
  | App(Variant(x, _), ys) -> x
  | Poly(xs, t) -> ocaml_of t      
  | App(TyFun([], t), []) -> ocaml_of t
  | App(NameTycon(x, _), ts) -> (String.concat " * " (List.map ocaml_of ts)) ^ " " ^ x
  | t -> Printf.eprintf "%s : not implemented yet." (string_of_t t); assert false
and ocaml_of_tycon = function
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | String -> "string"
  | t -> Printf.eprintf "%s : not implemented yet." (string_of_tycon t); assert false

(* 等値判定。型推論後のみ使用可能。*)
let rec equal t1 t2 = 
  match t1, t2 with
  | App(Unit, xs), App(Unit, ys) 
  | App(Bool, xs), App(Bool, ys) 
  | App(Int, xs), App(Int, ys) 
  | App(Arrow, xs), App(Arrow, ys) 
  | App(Tuple, xs), App(Tuple, ys) when List.length xs = List.length ys -> List.for_all2 equal xs ys
  | App(Record(x, _), xs), App(Record(y, _), ys) 
  | App(Variant(x, _), xs), App(Variant(y, _), ys) when List.length xs = List.length ys -> x = y && List.for_all2 equal xs ys
  | App(TyFun(xs, u), ys), t2 -> assert false (* inpossible after Typing.f *)
  | Poly([], u1), t2 -> equal u1 t2
  | t1, Poly([], u2) -> equal t1 u2
  | Poly(xs, u1), Poly(ys, u2) -> xs = ys && equal u1 u2
  | Var(x), Var(y) -> true
  | Field(_, x), Field(_, y) -> equal x y
  | Meta{ contents = Some(t1') }, t2 -> equal t1' t2
  | Meta(x), Meta{ contents = Some(t2') } -> equal t1 t2'
  | Meta(x), Meta(y) when x == y -> true
  | Meta(x), t2 -> assert false (* inpossible after Typing.f *)
  | t1, Meta(y) -> equal t2 t1
  | _, _ -> false
      
(* 型環境 venv に追加する識別子と型のリスト *)
let vars t =
  Log.debug "# Types.vars %s\n" (string_of_tycon t);
  match t with
  | TyFun(xs, (App(Variant(x, constrs), _) as t)) -> 
      List.map 
        (function
        | y, [] -> y, Poly(xs, t)
        | y, ts -> y, Poly(xs, App(Arrow, ts @ [t]))) constrs
  | _ -> []

(* 型環境 tenv に追加する識別子と型のリスト *)
let types t =
  Log.debug "# Types.types %s\n" (string_of_tycon t);
  match t with
  | TyFun(xs, (App(Record(x, fs), ys) as t)) -> (List.combine fs (List.map (fun y -> (Poly(xs, Field(t, y))))  ys))
  | _ -> []

let rec name t =
  match t with
  | App(Unit, []) -> "unit"
  | App(Bool, []) -> "bool"
  | App(Int, []) -> "int"
  | App(String, []) -> "string"
  | App(Atom, []) -> "atom"
  | App(Arrow, ts) -> "fun_of_" ^ (String.concat "_" (List.map name ts))
  | App(Tuple, ts) -> "tuple_of_" ^ (String.concat "_" (List.map name ts))
  | App(Module x, []) -> x
  | App(Record(x, _), _) -> x
  | App(Variant(x, _), _) -> x
  | Field(_, t) -> name t
  | App(TyFun([], t), []) -> name t
  | Var _ | Poly _ | Meta _ | App(Unit, _) | App(Bool, _) | App(Int, _)
  | App(String, _) | App(Atom, _) | App(TyFun _, _) | App(Module _, _) ->
    assert false (* impossible *)
  | App(NameTycon(x, _), _) -> x

let app_unit = App (Unit, [])
