type typ =
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

and inst =
  | I_seq of inst * inst
  | I_drop
  | I_drop_n of Z.t
  | I_dup
  | I_swap
  | I_dig of Z.t
  | I_dug of Z.t
  | I_push of typ * data
  | I_some
  | I_none of typ
  | I_unit
  | I_if_none of inst * inst
  | I_if_some of inst * inst
  | I_pair
  | I_car
  | I_cdr
  | I_left of typ
  | I_right of typ
  | I_if_left of inst * inst
  | I_if_right of inst * inst
  | I_nil of typ
  | I_cons
  | I_if_cons of inst * inst
  | I_size
  | I_empty_set of typ
  | I_empty_map of typ * typ
  | I_empty_big_map of typ * typ
  | I_map of inst
  | I_iter of inst
  | I_mem
  | I_get
  | I_update
  | I_if of inst * inst
  | I_loop of inst
  | I_loop_left of inst
  | I_lambda of typ * typ * inst
  | I_exec
  | I_dip of inst
  | I_dip_n of Z.t * inst
  | I_failwith
  | I_cast of typ
  | I_rename
  | I_concat
  | I_slice
  | I_pack
  | I_unpack of typ
  | I_add
  | I_sub
  | I_mul
  | I_ediv
  | I_abs
  | I_isnat
  | I_int
  | I_neg
  | I_lsl
  | I_lsr
  | I_or
  | I_and
  | I_xor
  | I_not
  | I_compare
  | I_eq
  | I_neq
  | I_lt
  | I_gt
  | I_le
  | I_ge
  | I_self
  | I_contract of typ
  | I_transfer_tokens
  | I_set_delegate
  | I_create_contract of program
  | I_implicit_account
  | I_now
  | I_amount
  | I_balance
  | I_check_signature
  | I_blake2b
  | I_sha256
  | I_sha512
  | I_hash_key
  | I_source
  | I_sender
  | I_address
  | I_chain_id
  | I_noop
  | I_unpair

and data =
  | D_int of Z.t
  | D_string of string
  | D_bytes of string
  | D_unit
  | D_bool of bool
  | D_pair of data * data
  | D_left of data
  | D_right of data
  | D_some of data
  | D_none
  | D_elt of data * data
  | D_list of data list

and program = { param : typ; storage : typ; code : inst }

(* val data_of_parser_data : typ -> parser_data -> data *)

val num_of_string : string -> Z.t

val num_of_int : int -> Z.t

val is_comparable_type : typ -> bool

val assert_type : data -> typ -> bool
