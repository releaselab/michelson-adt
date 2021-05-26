open Base

type annot = A_type of string | A_var of string | A_field of string
[@@deriving eq, ord, sexp]

type 'a node = {
  value : 'a; [@main]
  annots : annot list; [@default []]
  loc : Micheline.Loc.t; [@default Micheline.Loc.dummy_location]
}
[@@deriving eq, ord, sexp, make]

type typ_t =
  | T_key
  | T_unit
  | T_signature
  | T_option of typ
  | T_list of typ
  | T_set of typ
  | T_operation
  | T_contract of typ
  | T_pair of typ * typ
  | T_or of typ * typ
  | T_lambda of typ * typ
  | T_map of typ * typ
  | T_big_map of typ * typ
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

and typ = typ_t node [@@deriving eq, ord, sexp]

type inst_t =
  | I_noop
  | I_failwith
  | I_seq of inst list
  | I_if of inst * inst
  | I_loop of inst
  | I_loop_left of inst
  | I_dip of inst
  | I_dip_n of Bigint.t * inst
  | I_exec
  | I_apply
  | I_drop
  | I_drop_n of Bigint.t
  | I_dup
  | I_swap
  | I_dig of Bigint.t
  | I_dug of Bigint.t
  | I_push of typ * data
  | I_unit
  | I_lambda of typ * typ * inst
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
  | I_empty_set of typ
  | I_mem
  | I_update
  | I_iter of inst
  | I_empty_map of typ * typ
  | I_get
  | I_map of inst
  | I_empty_big_map of typ * typ
  | I_some
  | I_none of typ
  | I_if_none of inst * inst
  | I_left of typ
  | I_right of typ
  | I_if_left of inst * inst
  | I_cons
  | I_nil of typ
  | I_if_cons of inst * inst
  | I_create_contract of program
  | I_transfer_tokens
  | I_set_delegate
  | I_balance
  | I_address
  | I_contract of typ
  | I_source
  | I_sender
  | I_self
  | I_amount
  | I_implicit_account
  | I_now
  | I_chain_id
  | I_pack
  | I_unpack of typ
  | I_hash_key
  | I_blake2b
  | I_sha256
  | I_sha512
  | I_check_signature
  | I_cast of typ
  | I_unpair
  | I_rename
[@@deriving ord, sexp]

and inst = inst_t node [@@deriving ord, sexp]

and data_t =
  | D_int of Bigint.t
  | D_string of string
  | D_bytes of Bytes.t
  | D_unit
  | D_bool of bool
  | D_pair of data * data
  | D_left of data
  | D_right of data
  | D_some of data
  | D_none
  | D_elt of data * data
  | D_list of data list
  | D_instruction of inst
[@@deriving ord, sexp]

and data = data_t node [@@deriving ord, sexp]

and program = { param : typ; storage : typ; code : inst } [@@deriving ord, sexp]

val annot_of_string : string -> annot option

type simple = unit

type annotated = annot list

type with_loc = Micheline.Loc.t

type with_loc_annotated = with_loc * annotated
