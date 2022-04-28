open! Core

type 'a t = { id : int; location : Loc.t; value : 'a } [@@deriving ord, sexp]

val create : int -> ?location:Loc.t -> 'a -> 'a t
