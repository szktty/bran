type t = {
  line : int;
  col : int;
  offset : int;
}

val zero : t

val of_lexing_pos : Lexing.position -> t
