val phys_equal : 'a -> 'a -> bool
val (==) : 'a -> 'a -> [`Consider_using_phys_equal]
val (!=) : 'a -> 'a -> [`Consider_using_phys_equal]

val opt_of_find : ('a -> 'b) -> 'a -> 'b option
val find_of_opt : 'a option -> 'a
