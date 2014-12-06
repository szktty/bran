open Base

exception Invalid_path

type t = Id.t list * Id.t

let of_list = function
  | [] -> assert false
  | [x] -> ([], x)
  | xs ->
    let xs' = List.rev xs in
    (List.rev & List.tl xs', List.hd xs')

let of_string s =
  match String.split (function '.' -> true | _ -> false) s with
  | [x] -> [], x
  | es ->
    let es' = List.rev es in
    List.rev & List.tl es', List.hd es'
