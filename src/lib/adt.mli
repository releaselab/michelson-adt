type annot = A_type of string | A_var of string | A_field of string

type ('l, 'a) typ_t =
  | T_key
  | T_unit
  | T_signature
  | T_option of ('l, 'a) typ
  | T_list of ('l, 'a) typ
  | T_set of ('l, 'a) typ
  | T_operation
  | T_contract of ('l, 'a) typ
  | T_pair of ('l, 'a) typ * ('l, 'a) typ
  | T_or of ('l, 'a) typ * ('l, 'a) typ
  | T_lambda of ('l, 'a) typ * ('l, 'a) typ
  | T_map of ('l, 'a) typ * ('l, 'a) typ
  | T_big_map of ('l, 'a) typ * ('l, 'a) typ
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
  | T_never
  | T_ticket of ('l, 'a) typ
  | T_bls12_381_g1
  | T_bls12_381_g2
  | T_bls12_381_fr
  | T_sapling_transaction of Z.t
  | T_sapling_state of Z.t

and ('l, 'a) typ = 'l * ('l, 'a) typ_t * 'a

and ('l, 'a) inst_t =
  | I_noop
  | I_failwith
  | I_seq of ('l, 'a) inst list
  | I_if of ('l, 'a) inst * ('l, 'a) inst
  | I_loop of ('l, 'a) inst
  | I_loop_left of ('l, 'a) inst
  | I_dip of ('l, 'a) inst
  | I_dip_n of Z.t * ('l, 'a) inst
  | I_exec
  | I_apply
  | I_drop
  | I_drop_n of Z.t
  | I_dup
  | I_dup_n of Z.t
  | I_swap
  | I_dig of Z.t
  | I_dug of Z.t
  | I_push of ('l, 'a) typ * ('l, 'a) data
  | I_unit
  | I_lambda of ('l, 'a) typ * ('l, 'a) typ * ('l, 'a) inst
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
  | I_empty_set of ('l, 'a) typ
  | I_mem
  | I_update
  | I_iter of ('l, 'a) inst
  | I_empty_map of ('l, 'a) typ * ('l, 'a) typ
  | I_get
  | I_map of ('l, 'a) inst
  | I_empty_big_map of ('l, 'a) typ * ('l, 'a) typ
  | I_some
  | I_none of ('l, 'a) typ
  | I_if_none of ('l, 'a) inst * ('l, 'a) inst
  | I_left of ('l, 'a) typ
  | I_right of ('l, 'a) typ
  | I_if_left of ('l, 'a) inst * ('l, 'a) inst
  | I_cons
  | I_nil of ('l, 'a) typ
  | I_if_cons of ('l, 'a) inst * ('l, 'a) inst
  | I_create_contract of ('l, 'a) program
  | I_transfer_tokens
  | I_set_delegate
  | I_balance
  | I_address
  | I_contract of ('l, 'a) typ
  | I_source
  | I_sender
  | I_self
  | I_amount
  | I_implicit_account
  | I_voting_power
  | I_now
  | I_chain_id
  | I_pack
  | I_unpack of ('l, 'a) typ
  | I_hash_key
  | I_blake2b
  | I_keccak
  | I_sha3
  | I_sha256
  | I_sha512
  | I_check_signature
  | I_cast of ('l, 'a) typ
  | I_unpair
  | I_rename
  | I_total_voting_power
  | I_pairing_check
  | I_sapling_empty_state of Z.t
  | I_sapling_verify_update
  | I_ticket
  | I_read_ticket
  | I_split_ticket
  | I_join_tickets
  | I_never
  | I_self_address
  | I_level
  | I_pair_n of Z.t
  | I_unpair_n of Z.t
  | I_get_n of Z.t
  | I_update_n of Z.t

and ('l, 'a) inst = 'l * ('l, 'a) inst_t * 'a

and ('l, 'a) data_t =
  | D_int of Z.t
  | D_string of string
  | D_bytes of Bytes.t
  | D_unit
  | D_bool of bool
  | D_pair of ('l, 'a) data * ('l, 'a) data
  | D_left of ('l, 'a) data
  | D_right of ('l, 'a) data
  | D_some of ('l, 'a) data
  | D_none
  | D_elt of ('l, 'a) data * ('l, 'a) data
  | D_list of ('l, 'a) data list
  | D_instruction of ('l, 'a) inst

and ('l, 'a) data = 'l * ('l, 'a) data_t

and ('l, 'a) program = {
  param : ('l, 'a) typ;
  storage : ('l, 'a) typ;
  code : ('l, 'a) inst;
}
