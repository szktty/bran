type t = desc Location.With.t

and desc =
  | Var of tyvar
  | Field of t * t (* レコードの型 * フィールドの型 *)
  | App of tycon * t list
  | Poly of tyvar list * t
  | Meta of t option ref (* 型推論であとで代入するために ref 型になっている *)

and tycon =
  | Unit
  | Bool
  | Int
  | Float
  | Char
  | String
  | Atom
  | Bitstring
  | Binary
  | Arrow
  | List
  | Tuple
  | Array
  | Record of Id.t * Id.t list (* 型名とフィールド識別子のリスト。型名はあとで名前引きやすいようにするため *)
  | Variant of Id.t * constr list (* 最初のId.tは型名。理由は同上 *)
  | TyFun of tyvar list * t
  | Instance of (tyvar * t) list * t
  | NameTycon of Id.t * tycon option ref 
  | Module of Id.t

and tyvar = Id.t
and metavar = Id.t
and constr = Id.t * t list


