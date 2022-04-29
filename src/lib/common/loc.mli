open! Core

type pos = { col : int; lin : int } [@@deriving ord, sexp]
type t = { filename : string; start_pos : pos; end_pos : pos }

include Comparable.S with type t := t
include Sexpable.S with type t := t

val dummy_loc : t
val to_string : t -> string
val pp : ?max_lines:int -> Format.formatter -> t -> unit
