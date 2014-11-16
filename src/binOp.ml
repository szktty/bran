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

let to_string = function
  | Add -> "+"
  | Sub -> "-"
  | FAdd -> "+."
  | FSub -> "-."
  | FMul -> "*."
  | FDiv -> "/."
  | Eq -> "="
  | LT -> "<"
  | LE -> "<="
  | GT -> ">"
  | GE -> ">="
  | SConcat -> "^"
