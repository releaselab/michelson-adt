type pos = { col : int; lin : int } [@@deriving ord, sexp]

type t = { filename : string; start_pos : pos; end_pos : pos }
[@@deriving ord, sexp]

val loc_of_lexbuf_positions : Lexing.position -> Lexing.position -> t
