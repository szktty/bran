(* customized version of Map *)

module M =
  Map.Make
    (struct
      type t = Id.t
      let compare = compare
    end)
include M

let add_list xys env = List.fold_left (fun env (x, y) -> add x y env) env xys
let add_list2 xs ys env = List.fold_left2 (fun env x y -> add x y env) env xs ys
let union m1 m2 = M.fold (fun k v env -> add k v env) m2 m1
let dump f = iter (fun x y -> Log.debug "%s -> %s\n" x (f y)) 
