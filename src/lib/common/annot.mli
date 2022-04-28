open! Core

type t = A_type of string | A_var of string | A_field of string

include Comparable.S with type t := t
include Sexpable.S with type t := t
