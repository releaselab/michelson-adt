open Base

type pos = { col : int; lin : int } [@@deriving ord, sexp]

type t = { filename : string; start_pos : pos; end_pos : pos }
[@@deriving ord, sexp]

let dummy_pos = { col = -1; lin = -1 }
let dummy_loc = { filename = ""; start_pos = dummy_pos; end_pos = dummy_pos }

let to_string l =
  let { filename; start_pos; _ } = l in
  Printf.sprintf "\"%s\", line %d, characters %d" filename start_pos.lin
    start_pos.col
