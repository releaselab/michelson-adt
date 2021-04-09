open Base

type pos = { col : int; lin : int } [@@deriving ord, sexp]

type t = { filename : string; start_pos : pos; end_pos : pos }
[@@deriving ord, sexp]
