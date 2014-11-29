let rec init = function
  | [] -> assert false
  | [x] -> []
  | (x::xs) -> x :: (init xs)
      
let for_all2 pred xs ys =  
  try 
    List.for_all2 pred xs ys
  with 
    Invalid_argument _ -> false
