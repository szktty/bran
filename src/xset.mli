module type S = sig
  include Spotlib.Xset.S
  val of_list : elt list -> t
end

module Make(O : Set.OrderedType) : S with type elt = O.t
