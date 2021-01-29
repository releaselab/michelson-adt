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

and typ = Location.t * typ_t

and inst_t =
  | I_noop
  | I_failwith
  | I_seq of inst list
  | I_if of inst * inst
  | I_loop of inst
  | I_loop_left of inst
  | I_dip of inst
  | I_dip_n of Z.t * inst
  | I_exec
  | I_apply
  | I_drop
  | I_drop_n of Z.t
  | I_dup
  | I_swap
  | I_dig of Z.t
  | I_dug of Z.t
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
  | I_if_some of inst * inst
  | I_left of typ
  | I_right of typ
  | I_if_left of inst * inst
  | I_if_right of inst * inst
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

and inst = Location.t * inst_t

and data_t =
  | D_int of Z.t
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

and data = Location.t * data_t

and program = { param : typ; storage : typ; code : inst }

let rec is_comparable_type =
  let is_simple_comparable_type (_, t) =
    match t with
    | T_int | T_nat | T_string | T_bytes | T_mutez | T_bool | T_key_hash
    | T_timestamp | T_address ->
        true
    | _ -> false
  in
  function
  | _, T_pair (t_1, t_2) ->
      is_simple_comparable_type t_1 && is_comparable_type t_2
  | t -> is_simple_comparable_type t

(* let rec data_of_parser_data (t, _) d =
  match d with
  | P_int d -> (
      match with
      | T_comparable (T_simple_comparable_type T_int) -> D_int d
      | T_comparable (T_simple_comparable_type T_nat) -> D_nat d
      | T_comparable (T_simple_comparable_type T_mutez) -> D_mutez d
      | _ -> assert false )
  | P_string s -> (
      match with
      | T_comparable (T_simple_comparable_type T_string) -> D_string s
      | T_comparable (T_simple_comparable_type T_key_hash) -> D_key_hash s
      | T_comparable (T_simple_comparable_type T_timestamp) -> D_timestamp s
      | T_comparable (T_simple_comparable_type T_address) -> D_address s
      | _ -> assert false )
  | P_bytes s -> (
      match with
      | T_comparable (T_simple_comparable_type T_bytes) -> D_bytes s
      | _ -> assert false )
  | P_bool b -> (
      match with
      | T_comparable (T_simple_comparable_type T_bool) -> D_bool b
      | _ -> assert false )
  | P_pair (d_1, d_2) -> (
      match with
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
      match with
      | T_or (t, _) -> D_left (data_of_parser_data d)
      | _ -> assert false )
  | P_right d -> (
      match with
      | T_or (_,) -> D_right (data_of_parser_data d)
      | _ -> assert false )
  | P_some d -> (
      match with
      | T_option -> D_some (data_of_parser_data d)
      | _ -> assert false )
  | P_none -> ( match with T_option -> D_none | _ -> assert false )
  | P_list d -> (
      match with
      | T_list -> D_list (t, List.map (data_of_parser_data) d)
      | T_set (t, a) ->
          let' = (T_comparable, a) in
          D_set ((t, a), List.map (data_of_parser_data') d)
      | _ -> assert false )
  | P_map d -> (
      match with
      | T_map ((t_1, a_1), t_2) ->
          D_map
            ( ((t_1, a_1), t_2),
              List.map
                (fun (k, v) ->
                  ( data_of_parser_data (T_comparable t_1, a_1) k,
                    data_of_parser_data t_2 v ))
                d )
      | _ -> assert false )
  | P_unit -> ( match with T_unit -> D_unit | _ -> assert false ) *)

let num_of_string = Z.of_string

let num_of_int = Z.of_int

let rec assert_type (_, d) (_, t) =
  match (d, t) with
  | D_int _, (T_int | T_nat | T_mutez)
  | D_unit, T_unit
  | D_none, T_option _
  | ( D_string _,
      (T_string | T_key | T_key_hash | T_signature | T_address | T_timestamp) )
  | D_bytes _, T_bytes
  | D_bool _, T_bool ->
      true
  | D_pair (d_1, d_2), T_pair (t_1, t_2) ->
      assert_type d_1 t_1 && assert_type d_2 t_2
  | D_left d', T_or (t', _) | D_right d', T_or (_, t') | D_some d', T_option t'
    ->
      assert_type d' t'
  | D_list l, (T_list t' | T_set t') ->
      List.for_all (fun d' -> assert_type d' t') l
  | D_list l, (T_map (k, v) | T_big_map (k, v)) ->
      let assert_type_map (_, d) k v =
        match d with
        | D_elt (d_k, d_v) -> assert_type d_k k && assert_type d_v v
        | _ -> false
      in
      List.for_all (fun d' -> assert_type_map d' k v) l
  | _ -> false
