open Spotlib.Base

type t = Id.t list * Id.t

let of_list = function
  | [] -> assert false
  | [x] -> ([], x)
  | xs ->
    let xs' = List.rev xs in
    (List.rev & List.tl xs', List.hd xs')
