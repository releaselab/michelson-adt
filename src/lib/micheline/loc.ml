open Base

type pos = { col : int; lin : int } [@@deriving eq, ord, sexp]

type t = { filename : string; start_pos : pos; end_pos : pos }
[@@deriving eq, ord, sexp]

let dummy_location =
  {
    filename = "";
    start_pos = { col = 0; lin = 0 };
    end_pos = { col = 0; lin = 0 };
  }

let loc_of_lexbuf_positions s e =
  let open Lexing in
  let filename = s.pos_fname in
  let col = s.pos_cnum - s.pos_bol + 1 in
  let start_pos = { col; lin = s.pos_lnum } in
  let col = e.pos_cnum - e.pos_bol + 1 in
  let end_pos = { col; lin = e.pos_lnum } in
  { filename; start_pos; end_pos }
