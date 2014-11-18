type t = 
  | Var of (Id.t * Type.t)
  | Tuple of Id.t * t list

let rec typ = function
  | Var (_, t) -> t
  | Tuple (_, es) -> Type.Tuple (List.map typ es)

let names args =
  let rec f = function
    | Var (x, _) -> [x]
    | Tuple (x, args') -> x :: (Spotlib.Xlist.concat_map f args')
  in
  Spotlib.Xlist.concat_map f args

let rec vars args =
  List.fold_left (fun accu arg ->
      match arg with
      | Var (x, t) -> (x, t) :: accu
      | Tuple (_, es) -> List.concat [vars es; accu]) [] args
