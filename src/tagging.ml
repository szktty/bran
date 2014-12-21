open Base

module type Tag = sig
  type t
end

module type S = sig

  type tag
  type +'a t

  val create : tag -> 'a -> 'a t
  val tag : 'a t -> tag
  val desc : 'a t -> 'a
  val set : 'a t -> 'a -> 'a t

  val tags : 'a t list -> tag list
  val descs : 'a t list -> 'a list
  val tags_descs: 'a t list -> tag list * 'a list
  val bindings : 'a t list -> (tag * 'a) list
  val map : (tag -> 'a -> 'b t) -> 'a t list -> 'b t list
  val mapi : (int -> tag -> 'a -> 'b t) -> 'a t list -> 'b t list
  val iter : (tag -> 'a -> unit) -> 'a t list -> unit
  val fold : ('a -> tag -> 'b -> 'a) -> 'a -> 'b t list -> 'a
  val concat : (tag -> tag -> tag) -> 'a t list -> 'a list t

end

module Make (T : Tag) = struct

  type tag = T.t

  type 'a t = {
    tag : T.t;
    desc : 'a;
  }

  let create tag desc = { tag; desc }

  let tag e = e.tag
  let desc e = e.desc
  let set e x = { e with desc = x }

  let tags es =
    List.rev & List.fold_left (fun accu e -> e.tag :: accu) [] es

  let descs es =
    List.rev & List.fold_left (fun accu e -> e.desc :: accu) [] es

  let tags_descs es =
    List.fold_left
      (fun (ts, ds) e -> e.tag :: ts, e.desc :: ds)
      ([], []) (List.rev es)

  let bindings es =
    List.rev & List.fold_left (fun accu e -> (e.tag, e.desc) :: accu) [] es

  let map f es =
    List.map (fun e -> f e.tag e.desc) es

  let mapi f es =
    List.mapi (fun i e -> f i e.tag e.desc) es

  let iter f es =
    List.iter (fun e -> f e.tag e.desc) es

  let fold f init es =
    List.fold_left (fun accu e -> f accu e.tag e.desc) init es

  let concat f es =
    match es with
    | [] -> failwith "Tagging.S.concat: empty list"
    | [e] -> create e.tag [e.desc]
    | e' :: es' ->
      let es'', t' = List.fold_left
          (fun (ds, t) e -> e.desc :: ds, f t e.tag)
          ([e'.desc], e'.tag) es'
      in
      create t' es''

end
