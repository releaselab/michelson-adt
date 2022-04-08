type pos = { col : int; lin : int } [@@deriving ord, sexp]

type t = { filename : string; start_pos : pos; end_pos : pos }
[@@deriving ord, sexp]

val dummy_loc : t
val to_string : t -> string
