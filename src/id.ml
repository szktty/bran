type t = string (* 変数の名前 (caml2html: id_t) *)

type l =
  | L of string (* top level functions *)
  | M of string (* module functions of Erlang *)

let wildcard = "_"

let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "%s%d" s !counter

let id_of_typ = function
  | Type.Unit -> "u"
  | Type.Bool -> "b"
  | Type.Int -> "i"
  | Type.Float -> "d"
  | Type.String -> "s"
  | Type.Fun _ -> "f"
  | Type.Tuple _ -> "t"
  | Type.List _ -> "l"
  | Type.Array _ -> "a" 
  | Type.Module _ -> "m" 
  | Type.Var _ -> assert false
let gentmp typ =
  incr counter;
  Printf.sprintf "_T%s%d" (id_of_typ typ) !counter
