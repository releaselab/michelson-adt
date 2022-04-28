open! Core

module T = struct
  type t = A_type of string | A_var of string | A_field of string
  [@@deriving ord, sexp]
end

include T
include Comparable.Make (T)
