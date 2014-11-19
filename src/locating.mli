type 'a t = Location.t * 'a

val create : Location.t -> 'a -> 'a t
val range : Location.t -> Location.t -> 'a -> 'a t

val loc : 'a t -> Location.t
val desc : 'a t -> 'a

val set : 'a t -> ('a -> 'b) -> 'b t
