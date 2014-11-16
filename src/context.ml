type t = {
  mutable mod_name : string;
  mutable top_funs : (Id.t * Id.t) list;
}

let create name = { mod_name = name; top_funs = []; }

let main = ref None

let get () =
  match !main with
  | None -> assert false
  | Some ctx -> ctx

let init_module name =
  main := Some (create name);
  Id.counter := 0

let modules_ref : Module.t list ref = ref []

let modules () = !modules_ref

let add_module t = modules_ref := t :: !modules_ref

let find_module_opt x =
  try Some (List.find (fun m -> m.Type.mod_name = x) !modules_ref)
  with Not_found -> None

let find_module x =
  match find_module_opt x with
  | None -> raise Not_found
  | Some m -> m

let current_module () =
  match !main with
  | None -> None
  | Some ctx -> find_module_opt ctx.mod_name

let top_funs () = (get ()).top_funs

let add_top_fun x x' =
  let ctx = get () in
  ctx.top_funs <- (x, x') :: ctx.top_funs

let typs = ref M.empty

let add_top_typ x t =
  typs := M.add x t !typs

let mem_top_typ x = M.mem x !typs

let find_top_typ x = M.find x !typs

let finish_module () =
  let ctx = get () in
  let es = M.fold (fun x t es -> (x, t) :: es) !typs [] in
  let m = Module.create ctx.mod_name es in
  modules_ref := m :: !modules_ref
