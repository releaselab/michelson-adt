open Base

type annot = string

type ('l, 'p) node =
  | Int of 'l * Bigint.t
  | String of 'l * string
  | Bytes of 'l * Bytes.t
  | Prim of 'l * 'p * ('l, 'p) node list * annot list
  | Seq of 'l * ('l, 'p) node list
[@@deriving sexp]
