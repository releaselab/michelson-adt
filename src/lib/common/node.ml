open! Core

type 'a t = {
  id : int; [@compare fun a b -> Int.compare a b]
  location : Loc.t;
  value : 'a; [@main]
}
[@@deriving ord, sexp]

let create id ?(location = Loc.dummy_loc) value = { id; location; value }
