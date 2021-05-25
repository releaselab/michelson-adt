open Base

type annot = string

type spec = Spec_pre | Spec_post

type ('l, 'p) node =
  | Int of 'l * Bigint.t
  | String of 'l * string
  | Bytes of 'l * Bytes.t
  | Prim of 'l * 'p * ('l, 'p) node list * annot list
  | Seq of 'l * ('l, 'p) node list * (spec * string) list
[@@deriving sexp]
