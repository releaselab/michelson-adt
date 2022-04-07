open Base
open Adt
open Typed_adt

exception Type_error of loc

let dummy_loc = Common.Loc.dummy_loc
let create_with_loc i = (dummy_loc, i, ())

let rec is_comparable_type = function
  | T_address | T_bool | T_bytes | T_chain_id | T_int | T_key | T_key_hash
  | T_mutez | T_nat | T_never | T_unit | T_string | T_signature | T_timestamp ->
      true
  | T_option t -> is_comparable_type t
  | T_or (t_1, t_2) -> is_comparable_type t_1 && is_comparable_type t_2
  | T_pair (t_1, t_2) -> is_comparable_type t_1 && is_comparable_type t_2
  | _ -> false

let rec is_packable = function
  | T_address | T_bls12_381_fr | T_bls12_381_g1 | T_bls12_381_g2 | T_bool
  | T_bytes | T_contract _ | T_int | T_key | T_key_hash | T_lambda _ | T_mutez
  | T_nat | T_never | T_sapling_transaction _ | T_signature | T_string
  | T_timestamp | T_unit | T_chain_id ->
      true
  | T_or (t_1, t_2) | T_map (t_1, t_2) | T_pair (t_1, t_2) ->
      is_packable t_1 && is_packable t_2
  | T_list t | T_option t | T_set t -> is_packable t
  | T_operation | T_chest | T_chest_key | T_ticket _
  | T_big_map (_, _)
  | T_sapling_state _ ->
      false

let rec is_contract_type_compatible contract_t t =
  match (contract_t, t) with
  | _ when equal_typ contract_t t -> true
  | T_pair (contract_1, contract_2), T_pair (t_1, t_2) ->
      is_contract_type_compatible contract_1 t_1
      && is_contract_type_compatible contract_2 t_2
  | T_or (t_1, t_2), _ ->
      is_contract_type_compatible t_1 t || is_contract_type_compatible t_2 t
  | T_contract c, T_contract t ->
      is_contract_type_compatible c t || is_contract_type_compatible t c
  | _ -> false

let rec type_typ (_loc, t, _) =
  match t with
  | Adt.T_address -> T_address
  | T_unit -> T_unit
  | T_never -> T_never
  | T_bool -> T_bool
  | T_int -> T_int
  | T_nat -> T_nat
  | T_string -> T_string
  | T_chain_id -> T_chain_id
  | T_bytes -> T_bytes
  | T_mutez -> T_mutez
  | T_key_hash -> T_key_hash
  | T_key -> T_key
  | T_signature -> T_signature
  | T_timestamp -> T_timestamp
  | T_operation -> T_operation
  | T_bls12_381_g1 -> T_bls12_381_g1
  | T_bls12_381_g2 -> T_bls12_381_g2
  | T_bls12_381_fr -> T_bls12_381_fr
  | T_chest -> T_chest
  | T_chest_key -> T_chest_key
  | T_option t -> T_option (type_typ t)
  | T_list t -> T_list (type_typ t)
  | T_set t -> T_set (type_typ t)
  | T_contract t -> T_contract (type_typ t)
  | T_ticket t -> T_ticket (type_typ t)
  | T_pair (t_1, t_2) -> T_pair (type_typ t_1, type_typ t_2)
  | T_or (t_1, t_2) -> T_or (type_typ t_1, type_typ t_2)
  | T_lambda (t_1, t_2) -> T_lambda (type_typ t_1, type_typ t_2)
  | T_map (t_1, t_2) -> T_map (type_typ t_1, type_typ t_2)
  | T_big_map (t_1, t_2) -> T_big_map (type_typ t_1, type_typ t_2)
  | T_sapling_transaction n -> T_sapling_transaction n
  | T_sapling_state n -> T_sapling_state n

(* ABS *************)

let type_abs loc stack =
  match Stack.pop stack with
  | Some T_int ->
      Stack.push stack T_nat;
      I_abs
  | _ -> raise (Type_error loc)

(* DROP *************)

let type_drop loc stack =
  match Stack.pop stack with
  | Some _ -> I_drop Bigint.one
  | None -> raise (Type_error loc)

(* DUP *)

let type_dup loc stack =
  match Stack.top stack with
  | Some x ->
      Stack.push stack x;
      I_dup
  | _ -> raise (Type_error loc)

(* SWAP *)

let type_swap loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some x, Some y ->
      Stack.push stack x;
      Stack.push stack y;
      I_swap
  | _ -> raise (Type_error loc)

(* UNIT *)

let type_unit _loc stack =
  Stack.push stack T_unit;
  I_unit

(****************************************************************************)

(* EQ *)

let type_eq loc stack =
  match Stack.pop stack with
  | Some T_int ->
      Stack.push stack T_bool;
      I_eq
  | _ -> raise (Type_error loc)

(* NEQ *)

let type_neq loc stack =
  match Stack.pop stack with
  | Some T_int ->
      Stack.push stack T_bool;
      I_neq
  | _ -> raise (Type_error loc)

(* LT *)

let type_lt loc stack =
  match Stack.pop stack with
  | Some T_int ->
      Stack.push stack T_bool;
      I_lt
  | _ -> raise (Type_error loc)

(* GT *)

let type_gt loc stack =
  match Stack.pop stack with
  | Some T_int ->
      Stack.push stack T_bool;
      I_gt
  | _ -> raise (Type_error loc)

(* LE *)

let type_le loc stack =
  match Stack.pop stack with
  | Some T_int ->
      Stack.push stack T_bool;
      I_le
  | _ -> raise (Type_error loc)

(* GE *)

let type_ge loc stack =
  match Stack.pop stack with
  | Some T_int ->
      Stack.push stack T_bool;
      I_ge
  | _ -> raise (Type_error loc)

(* OR *)

let type_or loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some T_bool, Some T_bool ->
      Stack.push stack T_bool;
      I_or_bool
  | Some T_nat, Some T_nat ->
      Stack.push stack T_nat;
      I_or_nat
  | _ -> raise (Type_error loc)

(* AND *)

let type_and loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some T_bool, Some T_bool ->
      Stack.push stack T_bool;
      I_and_bool
  | Some T_nat, Some T_nat ->
      Stack.push stack T_nat;
      I_and_nat
  | Some T_int, Some T_nat ->
      Stack.push stack T_nat;
      I_and_int_nat
  | _ -> raise (Type_error loc)

(* XOR *)

let type_xor loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some T_bool, Some T_bool ->
      Stack.push stack T_bool;
      I_xor_bool
  | Some T_nat, Some T_nat ->
      Stack.push stack T_nat;
      I_xor_nat
  | _ -> raise (Type_error loc)

(* NOT *)

let type_not loc stack =
  match Stack.pop stack with
  | Some T_bool ->
      Stack.push stack T_bool;
      I_not_bool
  | Some T_nat ->
      Stack.push stack T_int;
      I_not_nat
  | Some T_int ->
      Stack.push stack T_int;
      I_not_int
  | _ -> raise (Type_error loc)

(* NEG *)

let type_neg loc stack =
  match Stack.pop stack with
  | Some T_int ->
      Stack.push stack T_int;
      I_neg_int
  | Some T_nat ->
      Stack.push stack T_int;
      I_int_nat
  | Some T_bls12_381_fr ->
      Stack.push stack T_bls12_381_fr;
      I_neg_bls12_381_fr
  | Some T_bls12_381_g1 ->
      Stack.push stack T_bls12_381_g1;
      I_neg_bls12_381_g1
  | Some T_bls12_381_g2 ->
      Stack.push stack T_bls12_381_g2;
      I_neg_bls12_381_g2
  | _ -> raise (Type_error loc)

(* ISNAT *)

let type_isnat loc stack =
  match Stack.pop stack with
  | Some T_int ->
      Stack.push stack (T_option T_nat);
      I_isnat
  | _ -> raise (Type_error loc)

(****************************************************************************)

(* INT *)

let type_int loc stack =
  match Stack.pop stack with
  | Some T_nat ->
      Stack.push stack T_int;
      I_int_nat
  | Some T_bls12_381_fr ->
      Stack.push stack T_int;
      I_int_bls12_381_fr
  | _ -> raise (Type_error loc)

(* ADD *)

let type_add loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some T_nat, Some T_nat ->
      Stack.push stack T_nat;
      I_add_nat
  | Some T_nat, Some T_int | Some T_int, Some T_nat ->
      Stack.push stack T_int;
      I_add_nat_int
  | Some T_int, Some T_int ->
      Stack.push stack T_int;
      I_add_int
  | Some T_timestamp, Some T_int | Some T_int, Some T_timestamp ->
      Stack.push stack T_timestamp;
      I_add_timestamp_int
  | Some T_mutez, Some T_mutez ->
      Stack.push stack T_mutez;
      I_add_mutez
  | Some T_bls12_381_g1, Some T_bls12_381_g1 ->
      Stack.push stack T_bls12_381_g1;
      I_add_bls12_381_g1
  | Some T_bls12_381_g2, Some T_bls12_381_g2 ->
      Stack.push stack T_bls12_381_g2;
      I_add_bls12_381_g2
  | Some T_bls12_381_fr, Some T_bls12_381_fr ->
      Stack.push stack T_bls12_381_fr;
      I_add_bls12_381_fr
  | _ -> raise (Type_error loc)

(* SUB *)

let type_sub loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some T_nat, Some T_nat ->
      Stack.push stack T_int;
      I_sub_nat
  | Some T_nat, Some T_int | Some T_int, Some T_nat ->
      Stack.push stack T_int;
      I_sub_nat_int
  | Some T_int, Some T_int ->
      Stack.push stack T_int;
      I_sub_int
  | Some T_timestamp, Some T_int ->
      Stack.push stack T_timestamp;
      I_sub_timestamp_int
  | Some T_timestamp, Some T_timestamp ->
      Stack.push stack T_int;
      I_sub_timestamp
  | Some T_mutez, Some T_mutez ->
      Stack.push stack T_mutez;
      I_sub_mutez
  | _ -> raise (Type_error loc)

(* MUL *)

let type_mul loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some T_nat, Some T_nat ->
      Stack.push stack T_nat;
      I_mul_nat
  | Some T_nat, Some T_int | Some T_int, Some T_nat ->
      Stack.push stack T_int;
      I_mul_nat_int
  | Some T_int, Some T_int ->
      Stack.push stack T_int;
      I_mul_int
  | Some T_mutez, Some T_nat | Some T_nat, Some T_mutez ->
      Stack.push stack T_mutez;
      I_mul_mutez_nat
  | Some T_bls12_381_g1, Some T_bls12_381_fr ->
      Stack.push stack T_bls12_381_g1;
      I_mul_bls12_381_g1_bls12_381_fr
  | Some T_bls12_381_g2, Some T_bls12_381_fr ->
      Stack.push stack T_bls12_381_g2;
      I_mul_bls12_381_g2_bls12_381_fr
  | Some T_bls12_381_fr, Some T_bls12_381_fr ->
      Stack.push stack T_bls12_381_fr;
      I_mul_bls12_381_fr_bls12_381_fr
  | Some T_nat, Some T_bls12_381_fr | Some T_bls12_381_fr, Some T_nat ->
      Stack.push stack T_bls12_381_fr;
      I_mul_nat_bls12_381_fr
  | Some T_int, Some T_bls12_381_fr | Some T_bls12_381_fr, Some T_int ->
      Stack.push stack T_bls12_381_fr;
      I_mul_int_bls12_381_fr
  | _ -> raise (Type_error loc)

(* EDIV *)

let type_ediv loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some T_nat, Some T_nat ->
      Stack.push stack (T_option (T_pair (T_nat, T_nat)));
      I_ediv_nat
  | Some T_nat, Some T_int | Some T_int, Some T_nat ->
      Stack.push stack (T_option (T_pair (T_int, T_nat)));
      I_ediv_nat_int
  | Some T_int, Some T_int ->
      Stack.push stack (T_option (T_pair (T_int, T_nat)));
      I_ediv_int
  | Some T_mutez, Some T_nat ->
      Stack.push stack (T_option (T_pair (T_mutez, T_mutez)));
      I_ediv_mutez_nat
  | Some T_mutez, Some T_mutez ->
      Stack.push stack (T_option (T_pair (T_nat, T_mutez)));
      I_ediv_mutez
  | _ -> raise (Type_error loc)

(* LSL *)

let type_lsl loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some T_nat, Some T_nat ->
      Stack.push stack T_nat;
      I_lsl
  | _ -> raise (Type_error loc)

(* LSR *)

let type_lsr loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some T_nat, Some T_nat ->
      Stack.push stack T_nat;
      I_lsr
  | _ -> raise (Type_error loc)

(* COMPARE *)

let type_compare loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some t_1, Some t_2 when equal_typ t_1 t_2 ->
      if is_comparable_type t_1 && is_comparable_type t_2 then (
        Stack.push stack T_int;
        I_compare)
      else raise (Type_error loc)
  | _ -> raise (Type_error loc)

(* CONCAT *)

let type_concat loc stack =
  match Stack.pop stack with
  | Some (T_list T_string) ->
      Stack.push stack T_string;
      I_concat_list_string
  | Some (T_list T_bytes) ->
      Stack.push stack T_bytes;
      I_concat_list_bytes
  | Some T_string -> (
      match Stack.pop stack with
      | Some T_string ->
          Stack.push stack T_string;
          I_concat_string
      | _ -> raise (Type_error loc))
  | Some T_bytes -> (
      match Stack.pop stack with
      | Some T_bytes ->
          Stack.push stack T_bytes;
          I_concat_bytes
      | _ -> raise (Type_error loc))
  | _ -> raise (Type_error loc)

(* SIZE *)

let type_size loc stack =
  match Stack.pop stack with
  | Some (T_set _) ->
      Stack.push stack T_nat;
      I_size_set
  | Some (T_map _) ->
      Stack.push stack T_nat;
      I_size_map
  | Some (T_list _) ->
      Stack.push stack T_nat;
      I_size_list
  | Some T_string ->
      Stack.push stack T_nat;
      I_size_string
  | Some T_bytes ->
      Stack.push stack T_nat;
      I_size_bytes
  | _ -> raise (Type_error loc)

(* SLICE *)

let type_slice loc stack =
  match (Stack.pop stack, Stack.pop stack, Stack.pop stack) with
  | Some T_nat, Some T_nat, Some T_string ->
      Stack.push stack (T_option T_string);
      I_slice_string
  | Some T_nat, Some T_nat, Some T_bytes ->
      Stack.push stack (T_option T_bytes);
      I_slice_bytes
  | _ -> raise (Type_error loc)

(* PAIR *)

let type_pair loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some t_1, Some t_2 ->
      Stack.push stack (T_pair (t_1, t_2));
      I_pair
  | _ -> raise (Type_error loc)

(* CAR *)

let type_car loc stack =
  match Stack.pop stack with
  | Some (T_pair (t, _)) ->
      Stack.push stack t;
      I_car
  | _ -> raise (Type_error loc)

(* CDR *)

let type_cdr loc stack =
  match Stack.pop stack with
  | Some (T_pair (_, t)) ->
      Stack.push stack t;
      I_cdr
  | _ -> raise (Type_error loc)

(* MEM *)

let type_mem loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some t, Some (T_set t') when equal_typ t t' ->
      Stack.push stack T_bool;
      I_mem_set
  | Some t, Some (T_map (t', _)) when equal_typ t t' ->
      Stack.push stack T_bool;
      I_mem_map
  | Some t, Some (T_big_map (t', _)) when equal_typ t t' ->
      Stack.push stack T_bool;
      I_mem_big_map
  | _ -> raise (Type_error loc)

(****************************************************************)

(* UPDATE *)

let type_update loc stack =
  match (Stack.pop stack, Stack.pop stack, Stack.pop stack) with
  | Some t, Some T_bool, Some (T_set t') when equal_typ t t' ->
      Stack.push stack (T_set t');
      I_update_set
  | Some t, Some (T_option t_2), Some (T_map (t', t_2'))
    when equal_typ t t' && equal_typ t_2 t_2' ->
      Stack.push stack (T_map (t', t_2'));
      I_update_map
  | Some t, Some (T_option t_2), Some (T_big_map (t', t_2'))
    when equal_typ t t' && equal_typ t_2 t_2' ->
      Stack.push stack (T_big_map (t', t_2'));
      I_update_map
  | _ -> raise (Type_error loc)

(* GET *)

let type_get loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some t_1', Some (T_map (t_1, t_2)) when equal_typ t_1 t_1' ->
      Stack.push stack (T_option t_2);
      I_get_map
  | Some t_1', Some (T_big_map (t_1, t_2)) when equal_typ t_1 t_1' ->
      Stack.push stack (T_option t_2);
      I_get_big_map
  | _ -> raise (Type_error loc)

(* SOME *)

let type_some loc stack =
  match Stack.pop stack with
  | Some t ->
      Stack.push stack (T_option t);
      I_some
  | _ -> raise (Type_error loc)

(* NONE *)

let type_none _loc stack t =
  Stack.push stack (T_option t);
  I_none t

(* CONS *)

let type_cons loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some t', Some (T_list t) when equal_typ t t' ->
      Stack.push stack (T_list t');
      I_cons
  | _ -> raise (Type_error loc)

(* TRANSFER_TOKENS *)

let type_transfer_tokens loc stack =
  match (Stack.pop stack, Stack.pop stack, Stack.pop stack) with
  | Some t, Some T_mutez, Some (T_contract t')
    when is_contract_type_compatible t' t ->
      Stack.push stack T_operation;
      I_transfer_tokens
  | _ -> raise (Type_error loc)

(****************************************************************)

(* SET_DELEGATE *)

let type_set_delegate loc stack =
  match Stack.pop stack with
  | Some (T_option T_key_hash) ->
      Stack.push stack T_operation;
      I_set_delegate
  | _ -> raise (Type_error loc)

(* BALANCE *)

let type_balance _loc stack =
  Stack.push stack T_mutez;
  I_balance

(* ADDRESS *)

let type_address loc stack =
  match Stack.pop stack with
  | Some (T_contract _) ->
      Stack.push stack T_address;
      I_address
  | _ -> raise (Type_error loc)

(* SOURCE *)

let type_source _loc stack =
  Stack.push stack T_address;
  I_source

(******************************************************************************)

(* SENDER *)

let type_sender _loc stack =
  Stack.push stack T_address;
  I_sender

(* SELF *)

let type_self _loc stack t =
  Stack.push stack (T_contract t);
  I_self

(* AMOUNT *)

let type_amount _loc stack =
  Stack.push stack T_mutez;
  I_amount

(* IMPLICIT_ACCOUNT *)

let type_implicit_account loc stack =
  match Stack.pop stack with
  | Some T_key_hash ->
      Stack.push stack (T_contract T_unit);
      I_implicit_account
  | _ -> raise (Type_error loc)

(* VOTING_POWER *)

let type_voting_power loc stack =
  match Stack.pop stack with
  | Some T_key_hash ->
      Stack.push stack T_nat;
      I_voting_power
  | _ -> raise (Type_error loc)

(* NOW *)

let type_now _loc stack =
  Stack.push stack T_timestamp;
  I_now

(* CHAIN_ID *)

let type_chain_id _loc stack =
  Stack.push stack T_chain_id;
  I_chain_id

(* PACK *)

let type_pack loc stack =
  match Stack.pop stack with
  | Some t ->
      if not (is_packable t) then raise (Type_error loc)
      else (
        Stack.push stack T_bytes;
        I_pack)
  | _ -> raise (Type_error loc)

(* HASH_KEY *)

let type_hash_key loc stack =
  match Stack.pop stack with
  | Some T_key ->
      Stack.push stack T_key_hash;
      I_hash_key
  | _ -> raise (Type_error loc)

(* BLAKE2B *)

let type_blake2b loc stack =
  match Stack.pop stack with
  | Some T_bytes ->
      Stack.push stack T_bytes;
      I_blake2b
  | _ -> raise (Type_error loc)

(* SHA3 *)

let type_sha3 loc stack =
  match Stack.pop stack with
  | Some T_bytes ->
      Stack.push stack T_bytes;
      I_sha3
  | _ -> raise (Type_error loc)

(* SHA256 *)

let type_sha256 loc stack =
  match Stack.pop stack with
  | Some T_bytes ->
      Stack.push stack T_bytes;
      I_sha256
  | _ -> raise (Type_error loc)

(* SHA512 *)

let type_sha512 loc stack =
  match Stack.pop stack with
  | Some T_bytes ->
      Stack.push stack T_bytes;
      I_sha512
  | _ -> raise (Type_error loc)

(* KECCAK *)

let type_keccak loc stack =
  match Stack.pop stack with
  | Some T_bytes ->
      Stack.push stack T_bytes;
      I_keccak
  | _ -> raise (Type_error loc)

(* CHECK_SIGNATURE *)

let type_check_signature loc stack =
  match (Stack.pop stack, Stack.pop stack, Stack.pop stack) with
  | Some T_key, Some T_signature, Some T_bytes ->
      Stack.push stack T_bool;
      I_check_signature
  | _ -> raise (Type_error loc)

(* TOTAL_VOTING_POWER *)

let type_total_voting_power _loc stack =
  Stack.push stack T_nat;
  I_total_voting_power

(****************************************************************)

(* PAIRING_CHECK *)

let type_pairing_check loc stack =
  match Stack.pop stack with
  | Some (T_list (T_pair (T_bls12_381_g1, T_bls12_381_g2))) ->
      Stack.push stack T_bool;
      I_pairing_check
  | _ -> raise (Type_error loc)

(* SAPLING_VERIFY_UPDATE *)

let type_sapling_verify_update loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some (T_sapling_transaction ms), Some (T_sapling_state ms')
    when Bigint.(ms = ms') ->
      Stack.push stack (T_option (T_pair (T_int, T_sapling_state ms)));
      I_sapling_verify_update
  | _ -> raise (Type_error loc)

(* SAPLING_EMPTY_STATE *)

let type_sapling_empty_state _loc stack ms =
  Stack.push stack (T_sapling_state ms);
  I_sapling_empty_state ms

(* TICKET *)

let type_ticket loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some t, Some T_nat when is_comparable_type t ->
      Stack.push stack (T_ticket t);
      I_ticket
  | _ -> raise (Type_error loc)

(* READ_TICKET *)

let type_read_ticket loc stack =
  match Stack.pop stack with
  | Some (T_ticket t) ->
      Stack.push stack (T_ticket t);
      Stack.push stack (T_pair (T_address, T_pair (t, T_nat)));
      I_read_ticket
  | _ -> raise (Type_error loc)

(* SPLIT_TICKET *)

let type_split_ticket loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some (T_ticket t), Some (T_pair (T_nat, T_nat)) ->
      Stack.push stack (T_option (T_pair (T_ticket t, T_ticket t)));
      I_split_ticket
  | _ -> raise (Type_error loc)

(* JOIN_TICKETS *)

let type_join_tickets loc stack =
  match Stack.pop stack with
  | Some (T_pair (T_ticket t, T_ticket t')) when equal_typ t t' ->
      Stack.push stack (T_option (T_ticket t));
      I_join_tickets
  | _ -> raise (Type_error loc)

let type_self_address _loc stack =
  Stack.push stack T_address;
  I_self_address

(* LEVEL *)

let type_level _loc stack =
  Stack.push stack T_nat;
  I_level

(* OPEN_CHEST *)

let type_open_chest loc stack =
  match (Stack.pop stack, Stack.pop stack, Stack.pop stack) with
  | Some T_chest_key, Some T_chest, Some T_nat ->
      Stack.push stack (T_or (T_bytes, T_bool));
      I_open_chest
  | _ -> raise (Type_error loc)

(* GET_AND_UPDATE *)

let type_get_and_update loc stack =
  match (Stack.pop stack, Stack.pop stack, Stack.pop stack) with
  | Some k, Some (T_option v), Some (T_map (k', v'))
    when equal_typ k k' && equal_typ v v' ->
      Stack.push stack (T_map (k, v));
      Stack.push stack (T_option v);
      I_get_and_update_map
  | Some k, Some (T_option v), Some (T_big_map (k', v'))
    when equal_typ k k' && equal_typ v v' ->
      Stack.push stack (T_big_map (k, v));
      Stack.push stack (T_option v);
      I_get_and_update_big_map
  | _ -> raise (Type_error loc)

(* NIL *)

let type_nil _loc stack t =
  Stack.push stack (T_list t);
  I_nil t

(* EMPTY_SET *)

let type_empty_set _loc stack t =
  Stack.push stack (T_set t);
  I_empty_set t

(* EMPTY_MAP *)

let type_empty_map _loc stack k v =
  Stack.push stack (T_map (k, v));
  I_empty_map (k, v)

(* EMPTY_BIG_MAP *)

let type_empty_big_map _loc stack k v =
  Stack.push stack (T_big_map (k, v));
  I_empty_big_map (k, v)

(* CREATE_CONTRACT *)

let type_create_contract loc stack p =
  match (Stack.pop stack, Stack.pop stack, Stack.pop stack) with
  | Some (T_option T_key_hash), Some T_mutez, Some g ->
      if equal_typ p.storage g then (
        Stack.push stack T_address;
        Stack.push stack T_operation;
        I_create_contract p)
      else raise (Type_error loc)
  | _ -> raise (Type_error loc)

(* CONTRACT *)

let type_contract loc stack t =
  match Stack.pop stack with
  | Some T_address ->
      Stack.push stack (T_option (T_contract t));
      I_contract t
  | _ -> raise (Type_error loc)

(****************************************************************************)

(* UNPACK *)

let type_unpack loc stack t =
  match Stack.pop stack with
  | Some T_bytes ->
      Stack.push stack (T_option t);
      I_unpack t
  | _ -> raise (Type_error loc)

(* CAST *)

let type_cast loc stack t =
  match Stack.pop stack with
  | Some t' when equal_typ t' t ->
      Stack.push stack t;
      I_cast t
  | _ -> raise (Type_error loc)

(* CREATE_ACCOUNT *)

let type_create_account loc stack =
  match
    (Stack.pop stack, Stack.pop stack, Stack.pop stack, Stack.pop stack)
  with
  | Some T_key_hash, Some (T_option T_key_hash), Some T_bool, Some T_mutez ->
      Stack.push stack T_address;
      Stack.push stack T_operation;
      I_create_account
  | _ -> raise (Type_error loc)

(****************************************************************************)

(* EXEC *)

let type_exec loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some a, Some (T_lambda (a', b)) when equal_typ a a' ->
      Stack.push stack b;
      I_exec
  | _ -> raise (Type_error loc)

(* APPLY *)

let type_apply loc stack =
  match (Stack.pop stack, Stack.pop stack) with
  | Some a, Some (T_lambda (T_pair (a', b), c)) when equal_typ a a' ->
      Stack.push stack (T_lambda (b, c));
      I_apply
  | _ -> raise (Type_error loc)

(* DROP n *)

let type_drop_n loc stack n =
  let rec aux i =
    if Bigint.(i = n) then ()
    else
      match Stack.pop stack with
      | Some _ -> aux Bigint.(i + one)
      | None -> raise (Type_error loc)
  in
  aux Bigint.zero;
  I_drop n

(* PUSH *)

let type_push _loc stack t d =
  Stack.push stack t;
  I_push (t, d)

(****************************************************************)

(* LEFT *)

let type_left loc stack t =
  match Stack.pop stack with
  | Some left ->
      Stack.push stack (T_or (left, t));
      I_left t
  | None -> raise (Type_error loc)

(* RIGHT *)

let type_right loc stack t =
  match Stack.pop stack with
  | Some right ->
      Stack.push stack (T_or (t, right));
      I_right t
  | None -> raise (Type_error loc)

(* UNPAIR n *)

let type_unpair loc stack n =
  let rec aux i x =
    if Bigint.(i = of_int 2) then
      match Stack.pop stack with
      | Some (T_pair (t_1, t_2)) ->
          Stack.push stack t_2;
          Stack.push stack t_1;
          List.iter x ~f:(fun x -> Stack.push stack x)
      | _ -> raise (Type_error loc)
    else
      match Stack.pop stack with
      | Some (T_pair (t_1, t_2)) ->
          Stack.push stack t_2;
          aux Bigint.(i - one) (t_1 :: x)
      | _ -> raise (Type_error loc)
  in
  aux n [];
  I_unpair n

(* DUP n *)

let type_dup_n loc stack n =
  let rec aux i x =
    if Bigint.(i = one) then
      match Stack.top stack with
      | Some t ->
          List.iter x ~f:(Stack.push stack);
          Stack.push stack t
      | None -> raise (Type_error loc)
    else
      match Stack.pop stack with
      | Some a -> aux Bigint.(i - one) (a :: x)
      | None -> raise (Type_error loc)
  in
  aux n [];
  I_dup_n n

(* DIG *)

let type_dig loc stack n =
  let rec aux i x =
    if Bigint.(i = zero) then
      match Stack.pop stack with
      | Some t ->
          List.iter x ~f:(fun x -> Stack.push stack x);
          Stack.push stack t
      | None -> raise (Type_error loc)
    else
      match Stack.pop stack with
      | Some a -> aux Bigint.(i - one) (a :: x)
      | None -> raise (Type_error loc)
  in
  aux n [];
  I_dig n

(* DUG n *)

let type_dug loc stack n =
  (if Bigint.(n = zero) then ()
  else
    match Stack.pop stack with
    | Some top ->
        let rec aux i x =
          match Stack.pop stack with
          | Some a ->
              if Bigint.(i = one) then (
                Stack.push stack top;
                Stack.push stack a;
                List.iter x ~f:(fun x -> Stack.push stack x))
              else aux Bigint.(i - one) (a :: x)
          | None -> raise (Type_error loc)
        in
        aux n []
    | None -> raise (Type_error loc));
  I_dug n

(* PAIR n *)

let type_pair_n loc stack n =
  let rec aux i l =
    if Bigint.(i = of_int 2) then
      match (Stack.pop stack, Stack.pop stack) with
      | Some t_1, Some t_2 ->
          let x = T_pair (t_1, t_2) in
          let x = List.fold_left l ~init:x ~f:(fun acc x -> T_pair (x, acc)) in
          Stack.push stack x
      | None, Some _ | Some _, None | None, None -> raise (Type_error loc)
    else
      match Stack.pop stack with
      | Some x -> aux Bigint.(i - one) (x :: l)
      | None -> raise (Type_error loc)
  in
  aux n [];
  I_pair_n n

(* GET n *)

let type_get_n loc stack n =
  let rec aux i =
    if Bigint.(i = zero) then ()
    else if Bigint.(i = one) then
      match Stack.pop stack with
      | Some (T_pair (a, _)) -> Stack.push stack a
      | _ -> raise (Type_error loc)
    else
      match Stack.pop stack with
      | Some (T_pair (_, y)) ->
          Stack.push stack y;
          aux Bigint.(i - one - one)
      | _ -> raise (Type_error loc)
  in
  aux n;
  I_get_n n

(* UPDATE n *)

let type_update_n loc stack n =
  match Stack.pop stack with
  | Some a -> (
      let rec aux i f x =
        if Bigint.(i = zero) then f a
        else if Bigint.(i = one) then
          match x with
          | T_pair (_, b) -> f (T_pair (a, b))
          | _ -> raise (Type_error loc)
        else
          match x with
          | T_pair (a, b) ->
              let f x = f (T_pair (a, x)) in
              aux Bigint.(i - one - one) f b
          | _ -> raise (Type_error loc)
      in
      match Stack.pop stack with
      | Some t ->
          let s = aux n (fun x -> x) t in
          Stack.push stack s;
          I_update_n n
      | None -> raise (Type_error loc))
  | _ -> raise (Type_error loc)

(****************************************************************)
let rec type_data t (loc, d) =
  match (t, d) with
  | T_bool, Adt.D_bool b -> (t, D_bool b)
  | T_int, D_int i | T_nat, D_int i | T_timestamp, D_int i | T_mutez, D_int i ->
      (t, D_int i)
  | T_bytes, D_bytes b -> (t, D_bytes b)
  | T_string, D_string s
  | T_timestamp, D_string s
  | T_contract _, D_string s
  | T_address, D_string s
  | T_key_hash, D_string s
  | T_signature, D_string s ->
      (t, D_string s)
  | T_unit, D_unit -> (t, D_unit)
  | T_option _, D_none -> (t, D_none)
  | T_option t', D_some d ->
      let d = type_data t' d in
      (t, D_some d)
  | T_pair (t_1, t_2), D_pair (d_1, d_2) ->
      let d_1 = type_data t_1 d_1 in
      let d_2 = type_data t_2 d_2 in
      (t, D_pair (d_1, d_2))
  | T_list t', D_list d_l ->
      let d = List.rev_map d_l ~f:(type_data t') in
      (t, D_list (List.rev d))
  | T_set t', D_list d_l ->
      let d = List.rev_map d_l ~f:(type_data t') in
      (t, D_list (List.rev d))
  | T_map (t_k, t_v), D_list d_l ->
      let type_elt t_k t_v (_, d) =
        match d with
        | Adt.D_elt (d_k, d_v) ->
            let d_k = type_data t_k d_k in
            let d_v = type_data t_v d_v in
            (d_k, d_v)
        | _ -> raise (Type_error loc)
      in
      let d_l = List.rev_map d_l ~f:(type_elt t_k t_v) in
      (t, D_map (List.rev d_l))
  | T_or (t', _), D_left d ->
      let d = type_data t' d in
      (t, D_left d)
  | T_or (_, t'), D_right d ->
      let d = type_data t' d in
      (t, D_right d)
  | T_lambda (p, r), D_instruction i -> (
      match type_lambda loc (Stack.create ()) p r i with
      | I_lambda (_, _, i) -> (t, D_instruction i)
      | _ -> assert false)
  | _ ->
      (* To debug: *)
      (* Stdio.print_endline (Sexp.to_string (Typed_adt.sexp_of_typ t));
         Stdio.print_endline (Sexp.to_string ([%sexp_of: _ Adt.data_t] d)); *)
      raise (Type_error loc)

and type_if loc p stack i_t i_f =
  match Stack.pop stack with
  | Some T_bool -> (
      let s_t = Stack.copy stack in
      let s_f = Stack.copy stack in
      let failed_t, i_t = type_inst p s_t i_t in
      let failed_f, i_f = type_inst p s_f i_f in
      let copy_stack () =
        if not failed_t then (
          Stack.clear stack;
          let l = Stack.fold s_t ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(Stack.push stack))
        else if not failed_f then (
          Stack.clear stack;
          let l = Stack.fold s_f ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(Stack.push stack))
      in
      let failed = failed_t && failed_f in
      let i = (failed, I_if (i_t, i_f)) in
      if failed then i
      else if failed_t || failed_f then (
        copy_stack ();
        i)
      else
        let l_s_t = Stack.to_list s_t in
        let l_s_f = Stack.to_list s_f in
        match List.for_all2 l_s_t l_s_f ~f:equal_typ with
        | List.Or_unequal_lengths.Ok b when b ->
            copy_stack ();
            i
        | _ -> raise (Type_error loc))
  | _ -> raise (Type_error loc)

and type_if_none loc p stack i_t i_f =
  match Stack.pop stack with
  | Some (T_option t) -> (
      let s_t = Stack.copy stack in
      let s_f = Stack.copy stack in
      Stack.push s_f t;
      let failed_t, i_t = type_inst p s_t i_t in
      let failed_f, i_f = type_inst p s_f i_f in
      let copy_stack () =
        if not failed_t then (
          Stack.clear stack;
          let l = Stack.fold s_t ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(Stack.push stack))
        else if not failed_f then (
          Stack.clear stack;
          let l = Stack.fold s_f ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(Stack.push stack))
      in
      let failed = failed_t && failed_f in
      let i = (failed, I_if_none (i_t, i_f)) in
      if failed then i
      else if failed_t || failed_f then (
        copy_stack ();
        i)
      else
        let l_s_t = Stack.to_list s_t in
        let l_s_f = Stack.to_list s_f in
        match List.for_all2 l_s_t l_s_f ~f:equal_typ with
        | List.Or_unequal_lengths.Ok b when b ->
            copy_stack ();
            i
        | _ -> raise (Type_error loc))
  | _ -> raise (Type_error loc)

and type_if_left loc p stack i_t i_f =
  match Stack.pop stack with
  | Some (T_or (t, f)) -> (
      let s_t = Stack.copy stack in
      let s_f = Stack.copy stack in
      Stack.push s_t t;
      Stack.push s_f f;
      let failed_t, i_t = type_inst p s_t i_t in
      let failed_f, i_f = type_inst p s_f i_f in
      let copy_stack () =
        if not failed_t then (
          Stack.clear stack;
          let l = Stack.fold s_t ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(Stack.push stack))
        else if not failed_f then (
          Stack.clear stack;
          let l = Stack.fold s_f ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(Stack.push stack))
      in
      let failed = failed_t && failed_f in
      let i = (failed, I_if_left (i_t, i_f)) in
      if failed then i
      else if failed_t || failed_f then (
        copy_stack ();
        i)
      else
        let l_s_t = Stack.to_list s_t in
        let l_s_f = Stack.to_list s_f in
        match List.for_all2 l_s_t l_s_f ~f:equal_typ with
        | List.Or_unequal_lengths.Ok b when b ->
            copy_stack ();
            i
        | _ -> raise (Type_error loc))
  | _ -> raise (Type_error loc)

and type_if_cons loc p stack i_t i_f =
  match Stack.pop stack with
  | Some (T_list t) -> (
      let s_t = Stack.copy stack in
      let s_f = Stack.copy stack in
      Stack.push s_t (T_list t);
      Stack.push s_t t;
      let failed_t, i_t = type_inst p s_t i_t in
      let failed_f, i_f = type_inst p s_f i_f in
      let copy_stack () =
        if not failed_t then (
          Stack.clear stack;
          let l = Stack.fold s_t ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(Stack.push stack))
        else if not failed_f then (
          Stack.clear stack;
          let l = Stack.fold s_f ~init:[] ~f:(fun acc t -> t :: acc) in
          List.iter l ~f:(Stack.push stack))
      in
      let failed = failed_t && failed_f in
      let i = (failed, I_if_cons (i_t, i_f)) in
      if failed then i
      else if failed_t || failed_f then (
        copy_stack ();
        i)
      else
        let l_s_t = Stack.to_list s_t in
        let l_s_f = Stack.to_list s_f in
        match List.for_all2 l_s_t l_s_f ~f:equal_typ with
        | List.Or_unequal_lengths.Ok b when b ->
            copy_stack ();
            i
        | _ -> raise (Type_error loc))
  | _ -> raise (Type_error loc)

and type_loop loc p stack b =
  match Stack.pop stack with
  | Some T_bool -> (
      let s = Stack.copy stack in
      let failed, b = type_inst p s b in
      let i = (failed, I_loop b) in
      if failed then i
      else
        match Stack.pop s with
        | Some T_bool -> (
            let l_stack = Stack.to_list stack in
            let l_s = Stack.to_list s in
            match List.for_all2 l_stack l_s ~f:equal_typ with
            | List.Or_unequal_lengths.Ok c when c -> i
            | _ -> raise (Type_error loc))
        | _ -> raise (Type_error loc))
  | _ -> raise (Type_error loc)

and type_loop_left loc p stack b =
  match Stack.pop stack with
  | Some (T_or (l, r)) -> (
      let s = Stack.copy stack in
      Stack.push s l;
      let failed, b = type_inst p s b in
      let i = (failed, I_loop_left b) in
      if failed then i
      else
        match Stack.pop s with
        | Some (T_or (l', r')) when equal_typ l l' && equal_typ r r' -> (
            let l_stack = Stack.to_list stack in
            let l_s = Stack.to_list s in
            match List.for_all2 l_stack l_s ~f:equal_typ with
            | List.Or_unequal_lengths.Ok c when c ->
                Stack.push stack r;
                i
            | _ -> raise (Type_error loc))
        | _ -> raise (Type_error loc))
  | _ -> raise (Type_error loc)

and type_iter loc p stack b =
  match Stack.pop stack with
  | Some (T_set t) -> (
      let s = Stack.copy stack in
      Stack.push s t;
      let failed, b = type_inst p s b in
      let i = (failed, I_iter_set b) in
      if failed then i
      else
        let l_stack = Stack.to_list stack in
        let l_s = Stack.to_list s in
        match List.for_all2 l_stack l_s ~f:equal_typ with
        | List.Or_unequal_lengths.Ok c when c -> i
        | _ -> raise (Type_error loc))
  | Some (T_map (k, v)) -> (
      let s = Stack.copy stack in
      Stack.push s (T_pair (k, v));
      let failed, b = type_inst p s b in
      let i = (failed, I_iter_map b) in
      if failed then i
      else
        let l_stack = Stack.to_list stack in
        let l_s = Stack.to_list s in
        match List.for_all2 l_stack l_s ~f:equal_typ with
        | List.Or_unequal_lengths.Ok c when c -> i
        | _ -> raise (Type_error loc))
  | Some (T_list t) -> (
      let s = Stack.copy stack in
      Stack.push s t;
      let failed, b = type_inst p s b in
      let i = (failed, I_iter_list b) in
      if failed then i
      else
        let l_stack = Stack.to_list stack in
        let l_s = Stack.to_list s in
        match List.for_all2 l_stack l_s ~f:equal_typ with
        | List.Or_unequal_lengths.Ok c when c -> i
        | _ -> raise (Type_error loc))
  | _ -> raise (Type_error loc)

and type_map loc p stack b =
  match Stack.pop stack with
  | Some (T_list t) -> (
      let s = Stack.copy stack in
      Stack.push s t;
      let failed, b = type_inst p s b in
      if failed then raise (Type_error loc)
      else
        let i = (false, I_map_list b) in
        match Stack.pop s with
        | Some t -> (
            let l_stack = Stack.to_list stack in
            let l_s = Stack.to_list s in
            match List.for_all2 l_stack l_s ~f:equal_typ with
            | List.Or_unequal_lengths.Ok c when c ->
                Stack.push stack (T_list t);
                i
            | _ -> raise (Type_error loc))
        | _ -> raise (Type_error loc))
  | Some (T_map (k, v)) -> (
      let s = Stack.copy stack in
      Stack.push s (T_pair (k, v));
      let failed, b = type_inst p s b in
      if failed then raise (Type_error loc)
      else
        let i = (false, I_map_map b) in
        match Stack.pop s with
        | Some t -> (
            let l_stack = Stack.to_list stack in
            let l_s = Stack.to_list s in
            match List.for_all2 l_stack l_s ~f:equal_typ with
            | List.Or_unequal_lengths.Ok c when c ->
                Stack.push stack (T_map (k, t));
                i
            | _ -> raise (Type_error loc))
        | _ -> raise (Type_error loc))
  | _ -> raise (Type_error loc)

and type_lambda loc stack p r b =
  let s = Stack.create () in
  Stack.push s p;
  let failed, b = type_inst p s b in
  let i = I_lambda (p, r, b) in
  Stack.push stack (T_lambda (p, r));
  if failed then i
  else
    match Stack.pop s with
    | Some r' ->
        if equal_typ r r' && Stack.is_empty s then i else raise (Type_error loc)
    | _ -> raise (Type_error loc)

and type_dip loc p stack i =
  match Stack.pop stack with
  | Some t ->
      let failed, i = type_inst p stack i in
      if failed then raise (Type_error loc)
      else
        let i = (false, I_dip i) in
        Stack.push stack t;
        i
  | None -> raise (Type_error loc)

and type_dip_n loc p stack n i =
  if Bigint.(of_int (Stack.length stack) < n) then raise (Type_error loc)
  else
    let l =
      List.init (Bigint.to_int_exn n) ~f:(fun _ ->
          match Stack.pop stack with
          | Some x -> x
          | None -> raise (Type_error loc))
    in
    let failed, i = type_inst p stack i in
    if failed then raise (Type_error loc)
    else
      let i = (false, I_dip_n (n, i)) in
      List.iter l ~f:(Stack.push stack);
      i

and type_seq p stack i_l =
  let type_inst = type_inst p stack in
  match i_l with
  | [] -> (false, I_noop)
  | x :: xs ->
      let failed, x = type_inst x in
      let failed, seq =
        List.fold_left xs ~init:(failed, [ x ]) ~f:(fun (failed, acc) ->
          function
          | (loc, _, _) as i ->
              if failed then raise (Type_error loc)
              else
                let failed, i = type_inst i in
                (failed, i :: acc))
      in
      (failed, I_seq (List.rev seq))

and type_program p =
  let s = Stack.create () in
  let param = type_typ p.Adt.param in
  let storage = type_typ p.Adt.storage in
  Stack.push s (T_pair (param, storage));
  let _, code = type_inst param s p.Adt.code in
  { param; storage; code }

and type_inst p stack (loc, i, _) : bool * Typed_adt.inst =
  match i with
  | Adt.I_abs -> (false, type_abs loc stack)
  | I_noop -> (false, I_noop)
  | I_drop -> (false, type_drop loc stack)
  | I_failwith -> (true, I_failwith)
  | I_dup -> (false, type_dup loc stack)
  | I_swap -> (false, type_swap loc stack)
  | I_unit -> (false, type_unit loc stack)
  | I_eq -> (false, type_eq loc stack)
  | I_neq -> (false, type_neq loc stack)
  | I_lt -> (false, type_lt loc stack)
  | I_gt -> (false, type_gt loc stack)
  | I_le -> (false, type_le loc stack)
  | I_ge -> (false, type_ge loc stack)
  | I_or -> (false, type_or loc stack)
  | I_and -> (false, type_and loc stack)
  | I_xor -> (false, type_xor loc stack)
  | I_not -> (false, type_not loc stack)
  | I_neg -> (false, type_neg loc stack)
  | I_isnat -> (false, type_isnat loc stack)
  | I_int -> (false, type_int loc stack)
  | I_add -> (false, type_add loc stack)
  | I_sub -> (false, type_sub loc stack)
  | I_mul -> (false, type_mul loc stack)
  | I_ediv -> (false, type_ediv loc stack)
  | I_lsl -> (false, type_lsl loc stack)
  | I_lsr -> (false, type_lsr loc stack)
  | I_compare -> (false, type_compare loc stack)
  | I_concat -> (false, type_concat loc stack)
  | I_size -> (false, type_size loc stack)
  | I_slice -> (false, type_slice loc stack)
  | I_pair -> (false, type_pair loc stack)
  | I_car -> (false, type_car loc stack)
  | I_cdr -> (false, type_cdr loc stack)
  | I_mem -> (false, type_mem loc stack)
  | I_update -> (false, type_update loc stack)
  | I_get -> (false, type_get loc stack)
  | I_some -> (false, type_some loc stack)
  | I_none t -> (false, type_none loc stack (type_typ t))
  | I_cons -> (false, type_cons loc stack)
  | I_transfer_tokens -> (false, type_transfer_tokens loc stack)
  | I_set_delegate -> (false, type_set_delegate loc stack)
  | I_balance -> (false, type_balance loc stack)
  | I_address -> (false, type_address loc stack)
  | I_source -> (false, type_source loc stack)
  | I_sender -> (false, type_sender loc stack)
  | I_self -> (false, type_self loc stack p)
  | I_amount -> (false, type_amount loc stack)
  | I_implicit_account -> (false, type_implicit_account loc stack)
  | I_voting_power -> (false, type_voting_power loc stack)
  | I_now -> (false, type_now loc stack)
  | I_chain_id -> (false, type_chain_id loc stack)
  | I_pack -> (false, type_pack loc stack)
  | I_hash_key -> (false, type_hash_key loc stack)
  | I_blake2b -> (false, type_blake2b loc stack)
  | I_keccak -> (false, type_keccak loc stack)
  | I_sha3 -> (false, type_sha3 loc stack)
  | I_sha256 -> (false, type_sha256 loc stack)
  | I_sha512 -> (false, type_sha512 loc stack)
  | I_check_signature -> (false, type_check_signature loc stack)
  | I_unpair -> (false, type_unpair loc stack Bigint.(one + one))
  | I_total_voting_power -> (false, type_total_voting_power loc stack)
  | I_pairing_check -> (false, type_pairing_check loc stack)
  | I_sapling_verify_update -> (false, type_sapling_verify_update loc stack)
  | I_ticket -> (false, type_ticket loc stack)
  | I_read_ticket -> (false, type_read_ticket loc stack)
  | I_split_ticket -> (false, type_split_ticket loc stack)
  | I_join_tickets -> (false, type_join_tickets loc stack)
  | I_never -> (true, I_never)
  | I_self_address -> (false, type_self_address loc stack)
  | I_level -> (false, type_level loc stack)
  | I_open_chest -> (false, type_open_chest loc stack)
  | I_get_and_update -> (false, type_get_and_update loc stack)
  | I_seq i_l -> type_seq p stack i_l
  | I_nil t -> (false, type_nil loc stack (type_typ t))
  | I_empty_set t -> (false, type_empty_set loc stack (type_typ t))
  | I_empty_map (k, v) ->
      (false, type_empty_map loc stack (type_typ k) (type_typ v))
  | I_empty_big_map (k, v) ->
      (false, type_empty_big_map loc stack (type_typ k) (type_typ v))
  | I_create_contract p ->
      (false, type_create_contract loc stack (type_program p))
  | I_contract t -> (false, type_contract loc stack (type_typ t))
  | I_unpack t -> (false, type_unpack loc stack (type_typ t))
  | I_cast t -> (false, type_cast loc stack (type_typ t))
  | I_sapling_empty_state ms -> (false, type_sapling_empty_state loc stack ms)
  | I_create_account -> (false, type_create_account loc stack)
  | I_if (i_t, i_f) -> type_if loc p stack i_t i_f
  | I_loop b -> type_loop loc p stack b
  | I_dip i -> type_dip loc p stack i
  | I_exec -> (false, type_exec loc stack)
  | I_apply -> (false, type_apply loc stack)
  | I_drop_n n -> (false, type_drop_n loc stack n)
  | I_push (t, d) ->
      let t = type_typ t in
      (false, type_push loc stack t (type_data t d))
  | I_left t -> (false, type_left loc stack (type_typ t))
  | I_right t -> (false, type_right loc stack (type_typ t))
  | I_unpair_n n -> (false, type_unpair loc stack n)
  | I_dip_n (n, i) -> type_dip_n loc p stack n i
  | I_dup_n n -> (false, type_dup_n loc stack n)
  | I_dig n -> (false, type_dig loc stack n)
  | I_dug n -> (false, type_dug loc stack n)
  | I_pair_n n -> (false, type_pair_n loc stack n)
  | I_get_n n -> (false, type_get_n loc stack n)
  | I_update_n n -> (false, type_update_n loc stack n)
  | I_if_none (i_t, i_f) -> type_if_none loc p stack i_t i_f
  | I_if_left (i_t, i_f) -> type_if_left loc p stack i_t i_f
  | I_if_cons (i_t, i_f) -> type_if_cons loc p stack i_t i_f
  | I_loop_left b -> type_loop_left loc p stack b
  | I_lambda (p, r, b) ->
      (false, type_lambda loc stack (type_typ p) (type_typ r) b)
  | I_iter b -> type_iter loc p stack b
  | I_map b -> type_map loc p stack b
  | I_rename -> (false, I_noop)
