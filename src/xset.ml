open Spotlib.Xset

module type S = sig
  include S
  val of_list : elt list -> t
end

module Make(O : Map.OrderedType) = struct
  include Make(O)

  let of_list l = List.fold_left (fun s e -> add e s) empty l

end
