type loc = Common_adt.Loc.t [@@deriving ord, sexp]

type 'a typ_t =
  | T_unit
  | T_never
  | T_bool
  | T_int
  | T_nat
  | T_string
  | T_chain_id
  | T_bytes
  | T_mutez
  | T_key_hash
  | T_key
  | T_signature
  | T_timestamp
  | T_address
  | T_option of 'a typ
  | T_list of 'a typ
  | T_set of 'a typ
  | T_operation
  | T_contract of 'a typ
  | T_ticket of 'a typ
  | T_pair of 'a typ * 'a typ
  | T_or of 'a typ * 'a typ
  | T_lambda of 'a typ * 'a typ
  | T_map of 'a typ * 'a typ
  | T_big_map of 'a typ * 'a typ
  | T_bls12_381_g1
  | T_bls12_381_g2
  | T_bls12_381_fr
  | T_sapling_transaction of Bigint.t
  | T_sapling_state of Bigint.t
  | T_chest
  | T_chest_key

and 'a typ = loc * 'a typ_t * 'a [@@deriving ord, sexp]

and 'a inst_t =
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
  | I_dup_n of Bigint.t
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
  | I_create_account
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
  | I_voting_power
  | I_now
  | I_chain_id
  | I_pack
  | I_unpack of 'a typ
  | I_hash_key
  | I_blake2b
  | I_keccak
  | I_sha3
  | I_sha256
  | I_sha512
  | I_check_signature
  | I_cast of 'a typ
  | I_unpair
  | I_unpair_n of Bigint.t
  | I_rename
  | I_total_voting_power
  | I_pairing_check
  | I_sapling_empty_state of Bigint.t
  | I_sapling_verify_update
  | I_ticket
  | I_read_ticket
  | I_split_ticket
  | I_join_tickets
  | I_never
  | I_self_address
  | I_level
  | I_pair_n of Bigint.t
  | I_get_n of Bigint.t
  | I_update_n of Bigint.t
  | I_open_chest
  | I_get_and_update

and 'a inst = loc * 'a inst_t * 'a [@@deriving ord, sexp]

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

and 'a data = loc * 'a data_t [@@deriving ord, sexp]

and 'a program = { param : 'a typ; storage : 'a typ; code : 'a inst }
[@@deriving ord, sexp]
