open X

let printf = function
  | true -> flush_all (); Printf.printf
  | false -> Printf.zprintf

let debug f = printf !Config.debug f
let verbose f = printf !Config.verbose f

let error f =
  Printf.printf "Error: ";
  Printf.printf f
