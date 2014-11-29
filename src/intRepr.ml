type t = int * string (* base and value *)

let to_int = function
  | 10, s -> int_of_string s
  | _ -> failwith "not implemented"

let to_string (b, v) =
  Printf.sprintf "%dr%s" b v 
