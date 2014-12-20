open Base

type 'a t = {
  loc : Location.t;
  desc : 'a;
}

let create loc desc = { loc; desc }

let range start_loc end_loc desc =
  create (Location.union start_loc end_loc) desc

let of_list es =
  Location.union (List.hd es).loc (List.last es).loc

let loc lx = lx.loc
let desc lx = lx.desc

let set lx x = { lx with desc = x }

let concat es =
  let (loc, es') =
    List.fold_left
      (fun (loc, accu) e -> (Location.union loc e.loc, e.desc :: accu))
      (Location.zero, []) es
  in
  create loc & List.rev es'

let values es =
  List.rev & List.fold_left (fun accu e -> e.desc :: accu) [] es
