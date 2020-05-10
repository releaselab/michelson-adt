type simple_comparable_type =
  | T_int
  | T_nat
  | T_string
  | T_bytes
  | T_mutez
  | T_bool
  | T_key_hash
  | T_timestamp
  | T_address

type simple_comparable_type_annotated = simple_comparable_type * string option

type comparable_type =
  | T_simple_comparable_type of simple_comparable_type
  | T_comparable_pair of
      simple_comparable_type_annotated * comparable_type_annotated

and comparable_type_annotated = comparable_type * string option

and typ =
  | T_comparable of comparable_type
  | T_key
  | T_unit
  | T_signature
  | T_option of typ_annotated
  | T_list of typ_annotated
  | T_set of comparable_type_annotated
  | T_operation
  | T_contract of typ_annotated
  | T_pair of typ_annotated * typ_annotated
  | T_or of typ_annotated * typ_annotated
  | T_lambda of typ_annotated * typ_annotated
  | T_map of comparable_type_annotated * typ_annotated
  | T_big_map of comparable_type_annotated * typ_annotated
  | T_chain_id

and typ_annotated = typ * string option

and _ typ_t =
  | Type : typ -> typ typ_t
  | Type_annot : typ_annotated -> typ_annotated typ_t
  | Comparable_type : comparable_type -> comparable_type typ_t
  | Comparable_type_annot :
      comparable_type_annotated
      -> comparable_type_annotated typ_t

and inst =
  | I_seq of inst_annotated * inst_annotated
  | I_drop
  | I_drop_n of Z.t
  | I_dup
  | I_swap
  | I_dig of Z.t
  | I_dug of Z.t
  | I_push of typ_annotated * data
  | I_some
  | I_none of typ_annotated
  | I_unit
  | I_if_none of inst_annotated * inst_annotated
  | I_pair
  | I_car
  | I_cdr
  | I_left of typ_annotated
  | I_right of typ_annotated
  | I_if_left of inst_annotated * inst_annotated
  | I_if_right of inst_annotated * inst_annotated
  | I_nil of typ_annotated
  | I_cons
  | I_if_cons of inst_annotated * inst_annotated
  | I_size
  | I_empty_set of comparable_type_annotated
  | I_empty_map of comparable_type_annotated * typ_annotated
  | I_empty_big_map of comparable_type_annotated * typ_annotated
  | I_map of inst_annotated
  | I_iter of inst_annotated
  | I_mem
  | I_get
  | I_update
  | I_if of inst_annotated * inst_annotated
  | I_loop of inst_annotated
  | I_loop_left of inst_annotated
  | I_lambda of typ_annotated * typ_annotated * inst_annotated
  | I_exec
  | I_dip of inst_annotated
  | I_dip_n of Z.t * inst_annotated
  | I_failwith
  | I_cast
  | I_rename
  | I_concat
  | I_slice
  | I_pack
  | I_unpack of typ_annotated
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
  | I_contract of typ_annotated
  | I_transfer_tokens
  | I_set_delegate
  | I_create_account
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
  | I_steps_to_quota
  | I_source
  | I_sender
  | I_address
  | I_chain_id
  | I_noop
  | I_unpair

and data =
  | D_int of Z.t
  | D_nat of Z.t
  | D_string of string
  | D_timestamp of string
  | D_signature of string
  | D_key of string
  | D_key_hash of string
  | D_mutez of Z.t
  | D_address of string
  | D_unit
  | D_bool of bool
  | D_pair of data * data
  | D_left of data
  | D_right of data
  | D_some of data
  | D_none of typ_annotated
  | D_list of typ_annotated * data list
  | D_set of comparable_type_annotated * data list
  | D_map of (comparable_type_annotated * typ_annotated) * (data * data) list
  (* | D_instruction of inst *)
  | D_bytes of string

and inst_annotated = inst * string list

and program = { param : typ_annotated; storage : typ_annotated; code : inst }

type parser_data =
  | P_int of Z.t
  | P_string of string
  | P_bytes of string
  | P_unit
  | P_bool of bool
  | P_pair of parser_data * parser_data
  | P_left of parser_data
  | P_right of parser_data
  | P_some of parser_data
  | P_none
  | P_map of (parser_data * parser_data) list
  | P_list of parser_data list

val data_of_parser_data : typ_annotated -> parser_data -> data

val num_of_string : string -> Z.t

val num_of_int : int -> Z.t
