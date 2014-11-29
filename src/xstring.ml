open Spotlib.Base

let concat_map sep f es =
  String.concat sep & List.map f es
