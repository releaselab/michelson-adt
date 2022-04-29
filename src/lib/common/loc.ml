open! Core

type pos = { col : int; lin : int } [@@deriving ord, sexp]

module T = struct
  type t = { filename : string; start_pos : pos; end_pos : pos }
  [@@deriving ord, sexp]
end

include T
include Comparable.Make (T)

let dummy_pos = { col = -1; lin = -1 }
let dummy_loc = { filename = ""; start_pos = dummy_pos; end_pos = dummy_pos }

let to_string l =
  let { filename; start_pos; _ } = l in
  Printf.sprintf "\"%s\", line %d, characters %d" filename start_pos.lin
    start_pos.col

let pp ?max_lines ppf l =
  let open Pp_loc in
  let input = Input.file l.filename in
  let l =
    ( Position.of_line_col l.start_pos.lin l.start_pos.col,
      Position.of_line_col l.end_pos.lin l.end_pos.col )
  in
  match max_lines with
  | None -> pp ~input ppf [ l ]
  | Some max_lines -> pp ~max_lines ~input ppf [ l ]
