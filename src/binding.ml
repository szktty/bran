open Base

exception Invalid_path

type t = C of t option * Id.t

let pervasives = C (None, "Pervasives")

let of_list = function
  | [] -> failwith "Binding.of_list"
  | [x] -> C (None, x)
  | x :: xs ->
    List.fold_left (fun path x -> C (Some path, x)) (C (None, x)) xs

let of_string s =
  of_list & String.split (function '.' -> true | _ -> false) s

let path_name (C (path, x)) = path, x
let path = fst ** path_name
let name = snd ** path_name

let to_list path =
  let rec f accu = function
    | C (None, x) -> x :: accu
    | C (Some path, x) -> x :: f accu path
  in
  List.rev & f [] path

let to_string = String.concat "." ** to_list

let to_erl_fun = function
  | C (None, x) -> x
  | C (Some path, x) ->
    Printf.sprintf "'%s':%s" (String.concat "." & to_list path) x

let to_erl_atom path =
  Printf.sprintf "'%s'" (String.concat "." & to_list path)
