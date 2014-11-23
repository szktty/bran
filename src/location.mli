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
val start_col : t -> int
val start_offset : t -> int
val end_line : t -> int
val end_col : t -> int
val end_offset : t -> int

val union : t -> t -> t

val contains_pos : t -> Position.t -> bool
val contains_offset : t -> int -> bool

val to_string : t -> string
