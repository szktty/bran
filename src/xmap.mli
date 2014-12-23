module type S = sig
  include Map.S
  val add_alist : (key * 'a) list -> 'a t -> 'a t
  val add_alist2 : key list -> 'a list -> 'a t -> 'a t
  val to_alist : 'a t -> (key * 'a) list
  val union : 'a t -> 'a t -> 'a t
  val to_string : (key -> string) -> ('a -> string) -> 'a t -> string
end

module Make(O : Map.OrderedType) : S with type key = O.t
