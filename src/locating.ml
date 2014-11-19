type 'a t = Location.t * 'a

let create loc desc = (loc, desc)

let range start_loc end_loc desc =
  create (Location.union start_loc end_loc) desc

let loc (l, _) = l
let desc (_, x) = x

let set ((l, x) : 'a t) f =
  create l (f x)
