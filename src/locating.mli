type 'a t = {
  loc : Location.t;
  desc : 'a;
}

val create : Location.t -> 'a -> 'a t
val range : Location.t -> Location.t -> 'a -> 'a t

val loc : 'a t -> Location.t
val desc : 'a t -> 'a

val set : 'a t -> 'b -> 'b t
