open Base

type annot = A_type of string | A_var of string | A_field of string
[@@deriving eq, ord, sexp]

let annot_of_string s =
  let a = String.sub s ~pos:1 ~len:(String.length s - 1) in
  match s.[0] with
  | '%' -> Some (A_field a)
  | '@' -> Some (A_var a)
  | ':' -> Some (A_type a)
  | _ -> None

type simple = unit

type annotated = annot list

type with_loc = Micheline.Loc.t

type with_loc_annotated = with_loc * annotated

type 'a typ_t =
  | T_key
  | T_unit
  | T_signature
  | T_option of 'a typ
  | T_list of 'a typ
  | T_set of 'a typ
  | T_operation
  | T_contract of 'a typ
  | T_pair of 'a typ * 'a typ
  | T_or of 'a typ * 'a typ
  | T_lambda of 'a typ * 'a typ
  | T_map of 'a typ * 'a typ
  | T_big_map of 'a typ * 'a typ
  | T_chain_id
  | T_int
  | T_nat
  | T_string
  | T_bytes
  | T_mutez
  | T_bool
  | T_key_hash
  | T_timestamp
  | T_address
[@@deriving eq, ord, sexp]

and 'a typ = 'a typ_t * 'a [@@deriving eq, ord, sexp]

type 'a inst_t =
  | I_noop
  | I_failwith
  | I_seq of 'a inst list
  | I_if of 'a inst * 'a inst
  | I_loop of 'a inst
  | I_loop_left of 'a inst
  | I_dip of 'a inst
  | I_dip_n of Bigint.t * 'a inst
  | I_exec
  | I_apply
  | I_drop
  | I_drop_n of Bigint.t
  | I_dup
  | I_swap
  | I_dig of Bigint.t
  | I_dug of Bigint.t
  | I_push of 'a typ * 'a data
  | I_unit
  | I_lambda of 'a typ * 'a typ * 'a inst
  | I_eq
  | I_neq
  | I_lt
  | I_gt
  | I_le
  | I_ge
  | I_or
  | I_and
  | I_xor
  | I_not
  | I_neg
  | I_abs
  | I_isnat
  | I_int
  | I_add
  | I_sub
  | I_mul
  | I_ediv
  | I_lsl
  | I_lsr
  | I_compare
  | I_concat
  | I_size
  | I_slice
  | I_pair
  | I_car
  | I_cdr
  | I_empty_set of 'a typ
  | I_mem
  | I_update
  | I_iter of 'a inst
  | I_empty_map of 'a typ * 'a typ
  | I_get
  | I_map of 'a inst
  | I_empty_big_map of 'a typ * 'a typ
  | I_some
  | I_none of 'a typ
  | I_if_none of 'a inst * 'a inst
  | I_left of 'a typ
  | I_right of 'a typ
  | I_if_left of 'a inst * 'a inst
  | I_cons
  | I_nil of 'a typ
  | I_if_cons of 'a inst * 'a inst
  | I_create_contract of 'a program
  | I_transfer_tokens
  | I_set_delegate
  | I_balance
  | I_address
  | I_contract of 'a typ
  | I_source
  | I_sender
  | I_self
  | I_amount
  | I_implicit_account
  | I_now
  | I_chain_id
  | I_pack
  | I_unpack of 'a typ
  | I_hash_key
  | I_blake2b
  | I_sha256
  | I_sha512
  | I_check_signature
  | I_cast of 'a typ
  | I_unpair
  | I_rename
[@@deriving ord, sexp]

and 'a inst = 'a inst_t * 'a [@@deriving ord, sexp]

and 'a data_t =
  | D_int of Bigint.t
  | D_string of string
  | D_bytes of Bytes.t
  | D_unit
  | D_bool of bool
  | D_pair of 'a data * 'a data
  | D_left of 'a data
  | D_right of 'a data
  | D_some of 'a data
  | D_none
  | D_elt of 'a data * 'a data
  | D_list of 'a data list
  | D_instruction of 'a inst
[@@deriving ord, sexp]

and 'a data = 'a data_t * 'a [@@deriving ord, sexp]

and 'a program = { param : 'a typ; storage : 'a typ; code : 'a inst }
[@@deriving ord, sexp]
