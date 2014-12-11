val init : 'a list -> 'a list
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val inject : (unit -> unit) -> ('a -> unit) -> 'a list -> unit
val inject2 : (unit -> unit) -> ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
