open Base

type t = {
  start : Position.t;
  end_ : Position.t;
  len : int;
}

let start_line t = t.start.line
let start_line1 t = t.start.line + 1
let start_col t = t.start.col
let start_col1 t = t.start.col + 1
let start_offset t = t.start.offset
let end_line t = t.end_.line
let end_line1 t = t.end_.line + 1
let end_col t = t.end_.col
let end_col1 t = t.end_.col + 1
let end_offset t = t.end_.offset

let values loc =
  (start_line loc), (start_col loc), (end_line loc), (end_col loc)

let values1 loc =
  (start_line1 loc), (start_col1 loc), (end_line1 loc), (end_col1 loc)

let create (start : Position.t) (end_ : Position.t) =
  let len = end_.offset - start.offset in
  if len < 0 then
    raise (Invalid_argument
             (Printf.sprintf "Location.create: start > end (%d, %d)"
                start.offset end_.offset))
  else
    { start; end_; len }

let zero =
  create Position.zero Position.zero

let _union start end_ =
  create start.start end_.end_

let union = _union

let contains_pos loc (pos : Position.t) =
  loc.start.offset <= pos.offset && pos.offset < loc.end_.offset

let contains_offset loc offset =
  loc.start.offset <= offset && offset < loc.end_.offset

let to_string loc =
  Printf.sprintf "%d:%d:%d-%d:%d:%d"
    loc.start.line loc.start.col loc.start.offset
    loc.end_.line loc.end_.col loc.end_.offset

type _t = t

module Tag_base = Tagging.Make(struct type t = _t end)

module Tag = struct
  include Tag_base

  let from_range start_loc end_loc value =
    create (_union start_loc end_loc) value

  let tag_of_list es =
    _union (tag & List.hd es) (tag & List.last es)

  let union es =
    concat (fun t1 t2 -> _union t1 t2) es

end
