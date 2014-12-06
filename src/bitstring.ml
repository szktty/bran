open Base

module Bits = struct

  type t = {
    value : value;
    size : int option;
    typ : typ;
    sign : sign option;
    endian : endian option;
    unit : int option;
  }

  and value =
    | Int of int
    | Float of float
    | String of string
    | Var of Id.t

  and typ = [`Int | `Float | `Binary | `Bitstring | `UTF8 | `UTF16 | `UTF32]
  and sign = [`Signed | `Unsigned]
  and endian = [`Big | `Little | `Native]

  let validate bits = 
    (* check size *)
    let size = 
      match bits.size with
      | None ->
        Some (begin match bits.typ with
          | `Int -> 8
          | `Float -> 64
          | `Binary | `Bitstring | _ -> failwith "not implemented" (* TODO *)
        end)
      | Some v -> Some v (* TODO: validation *)
    in

    (* check signedness *)
    let sign =
      Some (match bits.sign with
      | None -> `Unsigned
      | Some v ->
        match bits.typ with
        | `Int -> v
        | _ -> failwith "Signedness must be given for int")
    in

    (* check endianness and type *)
    let endian =
      Some (match bits.endian with
      | None -> `Big
      | Some v ->
        match bits.typ with
        | `Int | `Float | `UTF16 | `UTF32 -> v
        | _ -> failwith "Endianness must be given for int, float, utf16 and utf32")
    in

    (* check unit *)
    let unit = 
      Some (match bits.unit with
      | None ->
        begin match bits.typ with
          | `Int | `Float | `Bitstring -> 1
          | `Binary -> 8
          | _ -> failwith "Unit specifier must be given"
        end
      | Some v ->
        if not (1 <= v && v <= 256) then
          failwith "Unit allows range 1..256"
        else
          v)
    in
    { bits with size; sign; endian; unit }

  let create ?size ?(typ=`Int) ?sign ?endian ?unit value =
    { value; size; typ; sign; endian; unit }

  let to_string bits =
    "**" (* TODO *)

end


type t = Bits.t list

let create l = l

let to_string l =
  "<<" ^ (String.concat_map ", " Bits.to_string l) ^ ">>"

let length bs =
  List.fold_left (fun sum b ->
                    match b.Bits.size with
                    | None -> failwith "size is none"
                    | Some v -> sum + v) 0 bs
