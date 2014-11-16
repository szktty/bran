type t =
  | Add
  | Sub
  | FAdd
  | FSub
  | FMul
  | FDiv
  | Eq
  | LT
  | LE
  | GT
  | GE
  | SConcat (* "^" *)

val to_string : t -> string
