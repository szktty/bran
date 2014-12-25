open Map

module type S = sig
  include S
  val add_alist : (key * 'a) list -> 'a t -> 'a t
  val add_alist2 : key list -> 'a list -> 'a t -> 'a t
  val to_alist : 'a t -> (key * 'a) list
  val union : 'a t -> 'a t -> 'a t
  val to_string : (key -> string) -> ('a -> string) -> 'a t -> string
end

module Make(O : Map.OrderedType) = struct
  include Make(O)

  let add_alist xys m =
    List.fold_left (fun m (x, y) -> add x y m) m xys

  let add_alist2 xs ys m =
    List.fold_left2 (fun m x y -> add x y m) m xs ys

  let to_alist m =
    fold (fun k v accu -> (k, v) :: accu) m []

  let union m1 m2 =
    fold (fun k v m -> add k v m) m2 m1

  let to_string fk fv m =
    let kvs = fold (fun k v accu ->
        Printf.sprintf "%s = %s" (fk k) (fv v) :: accu) m []
    in
    "{" ^ (String.concat "; " kvs) ^ "}"

end
