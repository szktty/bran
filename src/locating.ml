open Base

type 'a t = {
  loc : Location.t;
  desc : 'a;
}

let create loc desc = { loc; desc }

let range start_loc end_loc desc =
  create (Location.union start_loc end_loc) desc

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
