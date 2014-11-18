exception Parse_error of Location.t * string

type t = 
  | Var of tyvar 
  | Field of t * t
  | App of tycon * t list 
  | Poly of tyvar list * t
  | Meta of t option ref
and tycon = 
  | Unit 
  | Bool 
  | Int 
  | Float
  | Arrow 
  | Tuple
  | Record of Id.t * Id.t list
  | Variant of Id.t * constr list
  | TyFun of tyvar list * t
  | NameTycon of Id.t * tycon option ref
and tyvar = Id.t
and metavar = Id.t
and constr = Id.t * t list
and module_ = {
  mod_name : string;
  mod_erl : string;
  mutable mod_funs : (string * fun_) list;
}
and fun_ = {
  fun_mod : module_ option;
  fun_ext : string option;
  fun_name : string option;
  fun_args : t list;
  fun_ret : t;
}

val newtyvar : unit -> Id.t
val newmetavar : unit -> t option ref

val string_of_t : t -> Id.t
val string_of_tycon : tycon -> Id.t
val string_of_constr : constr -> string
val ocaml_of : t -> Id.t

val equal : t -> t -> bool
val vars : tycon -> (Id.t * t) list
val types : tycon -> (Id.t * t) list

val prefix : t -> Id.t
val name : t -> Id.t
