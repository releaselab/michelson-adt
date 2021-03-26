open Base

type pos = { col : int; lin : int } [@@deriving ord, sexp]

type t = { s : pos; e : pos } [@@deriving ord, sexp]
