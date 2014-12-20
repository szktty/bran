type t = {
  start : Position.t;   (** 開始位置 *)
  end_ : Position.t;    (** 終了位置。終端の要素の次の位置。
                          * start と end_ が同じ位置であれば、
                          * 空の範囲であることを示す *)
  len : int;            (** 長さ。 start と end_ が同じ位置であれば 0 *)
}

val zero : t

val create : Position.t -> Position.t -> t

val start_line : t -> int
val start_line1 : t -> int
val start_col : t -> int
val start_col1 : t -> int
val start_offset : t -> int
val end_line : t -> int
val end_line1 : t -> int
val end_col : t -> int
val end_col1 : t -> int
val end_offset : t -> int

val values : t -> int * int * int * int
val values1 : t -> int * int * int * int

val union : t -> t -> t

val contains_pos : t -> Position.t -> bool
val contains_offset : t -> int -> bool

val to_string : t -> string

module With : sig

  type with_ = t

  type 'a t = {
    with_ : with_;
    desc : 'a;
  }

  val create : with_ -> 'a -> 'a t
  val range : with_ -> with_ -> 'a -> 'a t
  val of_list : 'a t list -> with_

  val with_ : 'a t -> with_
  val desc : 'a t -> 'a

  val set : 'a t -> 'b -> 'b t

  val concat : 'a t list -> 'a list t
  val values : 'a t list -> 'a list

end
