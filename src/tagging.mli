module type Tag = sig
  type t
end

module type S = sig

  type tag

  type +'a t = {
    tag : tag;
    desc : 'a;
  }

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

module Make (T : Tag) : S with type tag = T.t
