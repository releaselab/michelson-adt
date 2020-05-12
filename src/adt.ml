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
  | I_cast of typ_annotated
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

and inst_annotated = inst * string list

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

let rec data_of_parser_data (t, _) d =
  match d with
  | P_int d -> (
      match t with
      | T_comparable (T_simple_comparable_type T_int) -> D_int d
      | T_comparable (T_simple_comparable_type T_nat) -> D_nat d
      | T_comparable (T_simple_comparable_type T_mutez) -> D_mutez d
      | _ -> assert false )
  | P_string s -> (
      match t with
      | T_comparable (T_simple_comparable_type T_string) -> D_string s
      | T_comparable (T_simple_comparable_type T_key_hash) -> D_key_hash s
      | T_comparable (T_simple_comparable_type T_timestamp) -> D_timestamp s
      | T_comparable (T_simple_comparable_type T_address) -> D_address s
      | _ -> assert false )
  | P_bytes s -> (
      match t with
      | T_comparable (T_simple_comparable_type T_bytes) -> D_bytes s
      | _ -> assert false )
  | P_bool b -> (
      match t with
      | T_comparable (T_simple_comparable_type T_bool) -> D_bool b
      | _ -> assert false )
  | P_pair (d_1, d_2) -> (
      match t with
      | T_comparable (T_comparable_pair ((t_1, a_1), (t_2, a_2))) ->
          D_pair
            ( data_of_parser_data
                (T_comparable (T_simple_comparable_type t_1), a_1)
                d_1,
              data_of_parser_data (T_comparable t_2, a_2) d_2 )
      | T_pair (t_1, t_2) ->
          D_pair (data_of_parser_data t_1 d_1, data_of_parser_data t_2 d_2)
      | _ -> assert false )
  | P_left d -> (
      match t with
      | T_or (t, _) -> D_left (data_of_parser_data t d)
      | _ -> assert false )
  | P_right d -> (
      match t with
      | T_or (_, t) -> D_right (data_of_parser_data t d)
      | _ -> assert false )
  | P_some d -> (
      match t with
      | T_option t -> D_some (data_of_parser_data t d)
      | _ -> assert false )
  | P_none -> ( match t with T_option t -> D_none t | _ -> assert false )
  | P_list d -> (
      match t with
      | T_list t -> D_list (t, List.map (data_of_parser_data t) d)
      | T_set (t, a) ->
          let t' = (T_comparable t, a) in
          D_set ((t, a), List.map (data_of_parser_data t') d)
      | _ -> assert false )
  | P_map d -> (
      match t with
      | T_map ((t_1, a_1), t_2) ->
          D_map
            ( ((t_1, a_1), t_2),
              List.map
                (fun (k, v) ->
                  ( data_of_parser_data (T_comparable t_1, a_1) k,
                    data_of_parser_data t_2 v ))
                d )
      | _ -> assert false )
  | P_unit -> ( match t with T_unit -> D_unit | _ -> assert false )

(* 
  | T_key, P_string s -> D_key s
  | T_unit, P_unit -> D_unit
  | T_signature, P_string s -> D_signature s
  | T_option (t, _), P_some d -> D_some (data_of_parser_data t d)
  | T_option (t, _), P_none -> D_none
  | T_list (t, _), P_list d -> D_list (List.map (data_of_parser_data t) d)
  | T_set (t, _), P_list d -> D_set (List.map (data_of_parser_data (T_comparable t)) d)
  | T_operation 
  | T_unit, P_unit -> D_unit
  |  *)
