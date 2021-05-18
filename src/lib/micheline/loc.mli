type pos = { col : int; lin : int }

type t = { filename : string; start_pos : pos; end_pos : pos }
[@@deriving eq, ord, sexp]

val dummy_location : t

val loc_of_lexbuf_positions : Lexing.position -> Lexing.position -> t
