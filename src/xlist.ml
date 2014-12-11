let rec init = function
  | [] -> assert false
  | [x] -> []
  | (x::xs) -> x :: (init xs)
      
let for_all2 pred xs ys =  
  try 
    List.for_all2 pred xs ys
  with 
    Invalid_argument _ -> false

let rec inject f1 f2 = function
  | [] -> ()
  | [e] -> f2 e
  | e :: es ->
    f2 e;
    f1 ();
    inject f1 f2 es

let rec inject2 f1 f2 xs ys =
  match xs, ys with
  | [], [] -> ()
  | [], _ | _, [] -> raise (Invalid_argument "inject2")
  | [x], [y] -> f2 x y
  | x :: xs, y :: ys ->
    f2 x y;
    f1 ();
    inject2 f1 f2 xs ys
