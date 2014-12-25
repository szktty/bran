let phys_equal = (==)
let (==) _ _ = `Consider_using_phys_equal
let (!=) _ _ = `Consider_using_phys_equal

let opt_of_find f x = try Some (f x) with Not_found -> None

let find_of_opt = function
  | None -> raise Not_found
  | Some v -> v
