type t = { venv : Type.t M.t; tenv : Type.t M.t; tycons : Type.tycon M.t; }
val empty : t ref
val add_typ : t -> M.key -> Type.tycon -> t
val add_var : t -> M.key -> Type.t -> t
val exists_tycon : t -> M.key -> bool
val find_tycon : t -> M.key -> Type.tycon
val import : t -> Module.t -> t
