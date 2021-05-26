open Base

type 'a t = 'a list [@@deriving eq, ord, sexp]

val push : 'a t -> 'a -> 'a t

val pop : 'a t -> ('a * 'a t) option

val pop_n : 'a t -> int -> ('a t * 'a t) option

val peek : 'a t -> 'a option

val drop : 'a t -> 'a t option

val drop_n : 'a t -> int -> 'a t option

val swap : 'a t -> 'a t option

val dig : 'a t -> int -> 'a t option

val dug : 'a t -> int -> 'a t option

val dup : 'a t -> 'a t option
