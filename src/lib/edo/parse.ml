open Base
open Micheline
open Node
open Adt

exception Parse_error of string

let print_error_position lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Stdlib.Format.sprintf "Line: %d Position: %d" pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_program filename =
  let in_c = Stdio.In_channel.create filename in
  let lexbuf = Lexing.from_channel in_c in
  let comments = ref [] in
  let res =
    try Ok (Parser.start (Lexer.next_token comments) lexbuf, comments) with
    | Lexer.Lexical_error msg ->
        let error_msg =
          Stdlib.Format.sprintf "%s: %s@." (print_error_position lexbuf) msg
        in
        Error (Error.of_string error_msg)
    | Parser.Error ->
        let error_msg =
          Stdlib.Format.sprintf "%s: syntax error@."
            (print_error_position lexbuf)
        in
        Error (Error.of_string error_msg)
  in
  let () = Stdio.In_channel.close in_c in
  res

let print_error_position l =
  Stdlib.Format.sprintf "Line: %d Position: %d" l.Loc.start_pos.lin
    l.Loc.start_pos.col

let rec type_parse = function
  | Prim (l, t, args, annot) ->
      let t =
        match (t, args) with
        | "address", [] -> T_address
        | "big_map", [ t_k; t_v ] -> T_big_map (type_parse t_k, type_parse t_v)
        | "bool", [] -> T_bool
        | "bytes", [] -> T_bytes
        | "chain_id", [] -> T_chain_id
        | "contract", [ t ] -> T_contract (type_parse t)
        | "int", [] -> T_int
        | "key", [] -> T_key
        | "key_hash", [] -> T_key_hash
        | "lambda", [ t_in; t_out ] ->
            T_lambda (type_parse t_in, type_parse t_out)
        | "list", [ t ] -> T_list (type_parse t)
        | "map", [ t_k; t_v ] -> T_map (type_parse t_k, type_parse t_v)
        | "mutez", [] -> T_mutez (* | "never", [] -> T_never *)
        | "nat", [] -> T_nat
        | "operation", [] -> T_operation
        | "option", [ t ] -> T_option (type_parse t)
        | "or", [ t_1; t_2 ] -> T_or (type_parse t_1, type_parse t_2)
        | "pair", [ t_1; t_2 ] -> T_pair (type_parse t_1, type_parse t_2)
        | "set", [ t ] -> T_set (type_parse t)
        | "signature", [] -> T_signature
        | "string", [] -> T_string
        | "timestamp", [] -> T_timestamp
        | "unit", [] -> T_unit
        | "never", [] -> T_never
        | "bls12_381_g1", [] -> T_bls12_381_g1
        | "bls12_381_g2", [] -> T_bls12_381_g2
        | "bls12_381_fr", [] -> T_bls12_381_fr
        | "sapling_transaction", [ Int (_, n) ] -> T_sapling_transaction n
        | "sapling_state", [ Int (_, n) ] -> T_sapling_state n
        | _ ->
            let error_msg =
              Stdlib.Format.sprintf "%s: ill-formed type@."
                (print_error_position l)
            in
            raise (Parse_error error_msg)
      in
      (l, t, annot)
  | Int (l, _) | String (l, _) | Bytes (l, _) | Seq (l, _) ->
      let error_msg =
        Stdlib.Format.sprintf "%s: syntax error@." (print_error_position l)
      in
      raise (Parse_error error_msg)

let rec data_parse = function
  | Int (l, n) -> (l, D_int n)
  | Bytes (l, b) -> (l, D_bytes b)
  | String (l, s) -> (l, D_string s)
  | Prim (l, p, args, _) -> (
      ( l,
        match (p, args) with
        | "Unit", [] -> D_unit
        | "True", [] -> D_bool true
        | "False", [] -> D_bool false
        | "Pair", [ d_1; d_2 ] -> D_pair (data_parse d_1, data_parse d_2)
        | "Left", [ d ] -> D_left (data_parse d)
        | "Right", [ d ] -> D_right (data_parse d)
        | "Some", [ d ] -> D_some (data_parse d)
        | "None", [] -> D_none
        | "Elt", [ d_1; d_2 ] -> D_elt (data_parse d_1, data_parse d_2)
        | _ ->
            let error_msg =
              Stdlib.Format.sprintf "%s: unknown primitive@."
                (print_error_position l)
            in
            raise (Parse_error error_msg) ))
  | Seq (l, datas) -> (
      try (l, D_list (List.map datas ~f:data_parse))
      with Parse_error _ ->
        (l, D_instruction (l, I_seq (List.map datas ~f:inst_parse), [])))

and inst_parse = function
  | Prim (l, i, args, annot) ->
      let i =
        match (i, args) with
        | "APPLY", [] -> I_apply
        | "EXEC", [] -> I_exec
        | "FAILWITH", [] -> I_failwith
        | "IF", [ i_1; i_2 ] -> I_if (inst_parse i_1, inst_parse i_2)
        | "IF_CONS", [ i_1; i_2 ] -> I_if_cons (inst_parse i_1, inst_parse i_2)
        | "IF_LEFT", [ i_1; i_2 ] -> I_if_left (inst_parse i_1, inst_parse i_2)
        | "IF_RIGHT", [ i_1; i_2 ] -> I_if_left (inst_parse i_2, inst_parse i_1)
        | "IF_NONE", [ i_1; i_2 ] -> I_if_none (inst_parse i_1, inst_parse i_2)
        | "ITER", [ i ] -> I_iter (inst_parse i)
        | "LAMBDA", [ t_in; t_out; i ] ->
            I_lambda (type_parse t_in, type_parse t_out, inst_parse i)
        | "LOOP", [ i ] -> I_loop (inst_parse i)
        | "LOOP_LEFT", [ i ] -> I_loop_left (inst_parse i)
        | "DIG", [ Int (_, n) ] -> I_dig n
        | "DIP", [ i ] -> I_dip (inst_parse i)
        | "DIP", [ Int (_, n); i ] -> I_dip_n (n, inst_parse i)
        | "DUG", [ Int (_, n) ] -> I_dug n
        | "DUP", [] -> I_dup
        | "DROP", [] -> I_drop
        | "DROP", [ Int (_, n) ] -> I_drop_n n
        | "PUSH", [ t; d ] -> I_push (type_parse t, data_parse d)
        | "SWAP", [] -> I_swap
        | "ABS", [] -> I_abs
        | "ADD", [] -> I_add
        | "COMPARE", [] -> I_compare
        | "EDIV", [] -> I_ediv
        | "EQ", [] -> I_eq
        | "GE", [] -> I_ge
        | "GT", [] -> I_gt
        | "LE", [] -> I_le
        | "LT", [] -> I_lt
        | "INT", [] -> I_int
        | "ISNAT", [] -> I_isnat
        | "LSL", [] -> I_lsl
        | "LSR", [] -> I_lsr
        | "MUL", [] -> I_mul
        | "NEG", [] -> I_neg
        | "NEQ", [] -> I_neq
        | "SUB", [] -> I_sub
        | "AND", [] -> I_and
        | "NOT", [] -> I_not
        | "OR", [] -> I_or
        | "XOR", [] -> I_xor
        | "BLAKE2B", [] -> I_blake2b
        | "CHECK_SIGNATURE", [] -> I_check_signature
        | "HASH_KEY", [] -> I_hash_key
        | "SHA256", [] -> I_sha256
        | "SHA512", [] -> I_sha512
        | "ADDRESS", [] -> I_address
        | "AMOUNT", [] -> I_amount
        | "BALANCE", [] -> I_balance
        | "CHAIN_ID", [] -> I_chain_id
        | "CONTRACT", [ t ] -> I_contract (type_parse t)
        | "CREATE_CONTRACT", [ p ] -> I_create_contract (program_parse p)
        | "IMPLICIT_ACCOUNT", [] -> I_implicit_account
        | "NOW", [] -> I_now
        | "SELF", [] -> I_self
        | "SENDER", [] -> I_sender
        | "SET_DELEGATE", [] -> I_set_delegate
        | "SOURCE", [] -> I_source
        | "TRANSFER_TOKENS", [] -> I_transfer_tokens
        | "CAR", [] -> I_car
        | "CDR", [] -> I_cdr
        | "CONCAT", [] -> I_concat
        | "CONS", [] -> I_cons
        | "EMPTY_BIG_MAP", [ t_k; t_v ] ->
            I_empty_big_map (type_parse t_k, type_parse t_v)
        | "EMPTY_MAP", [ t_k; t_v ] ->
            I_empty_map (type_parse t_k, type_parse t_v)
        | "EMPTY_SET", [ t ] -> I_empty_set (type_parse t)
        | "GET", [] -> I_get
        | "LEFT", [ t ] -> I_left (type_parse t)
        | "MAP", [ i ] -> I_map (inst_parse i)
        | "MEM", [] -> I_mem
        | "NIL", [ t ] -> I_nil (type_parse t)
        | "NONE", [ t ] -> I_none (type_parse t)
        | "PACK", [] -> I_pack
        | "PAIR", [] -> I_pair
        | "RIGHT", [ t ] -> I_right (type_parse t)
        | "SIZE", [] -> I_size
        | "SLICE", [] -> I_slice
        | "SOME", [] -> I_some
        | "UNIT", [] -> I_unit
        | "UNPACK", [ t ] -> I_unpack (type_parse t)
        | "UPDATE", [] -> I_update
        | "UNPAIR", [] -> I_unpair
        | "NEVER", [] -> I_never
        | "SELF_ADDRESS", [] -> I_self_address
        | "VOTING_POWER", [] -> I_voting_power
        | "LEVEL", [] -> I_level
        | "KECCAK", [] -> I_keccak
        | "SHA3", [] -> I_sha3
        | "TOTAL_VOTING_POWER", [] -> I_total_voting_power
        | "PAIRING_CHECK", [] -> I_pairing_check
        | "SAPLING_VERIFY_UPDATE", [] -> I_sapling_verify_update
        | "TICKET", [] -> I_ticket
        | "READ_TICKET", [] -> I_read_ticket
        | "SPLIT_TICKET", [] -> I_split_ticket
        | "JOIN_TICKETS", [] -> I_join_tickets
        | "DUP", [ Int (_, n) ] -> I_dup_n n
        | "PAIR", [ Int (_, n) ] -> I_pair_n n
        | "GET", [ Int (_, n) ] -> I_get_n n
        | "UPDATE", [ Int (_, n) ] -> I_update_n n
        | "SAPLING_EMPTY_STATE", [ Int (_, n) ] -> I_sapling_empty_state n
        | _ ->
            let error_msg =
              Stdlib.Format.sprintf "%s: unknown primitive@."
                (print_error_position l)
            in
            raise (Parse_error error_msg)
      in
      (l, i, annot)
  | Int (l, _) | String (l, _) | Bytes (l, _) | Seq (l, _) ->
      let error_msg =
        Stdlib.Format.sprintf "%s: syntax error@." (print_error_position l)
      in
      raise (Parse_error error_msg)

and code_parse = function
  | Seq (l, insts) -> (l, I_seq (List.map insts ~f:inst_parse), [])
  | Int (l, _) | String (l, _) | Bytes (l, _) | Prim (l, _, _, _) ->
      let error_msg =
        Stdlib.Format.sprintf "%s: code of contract must be a sequence@."
          (print_error_position l)
      in
      raise (Parse_error error_msg)

and param_parse = type_parse

and storage_parse = type_parse

and program_parse = function
  | Seq (_l, nodes) -> (
      match nodes with
      | [
       (Prim (_, p_1, _, _) as prim_1);
       (Prim (_, p_2, _, _) as prim_2);
       (Prim (_, p_3, _, _) as prim_3);
      ] ->
          let code =
            if String.equal p_1 "code" then prim_1
            else if String.equal p_2 "code" then prim_2
            else if String.equal p_3 "code" then prim_3
            else raise (Parse_error "missing script code")
          in
          let param =
            if String.equal p_1 "parameter" then prim_1
            else if String.equal p_2 "parameter" then prim_2
            else if String.equal p_3 "parameter" then prim_3
            else raise (Parse_error "missing script parameter")
          in
          let storage =
            if String.equal p_1 "storage" then prim_1
            else if String.equal p_2 "storage" then prim_2
            else if String.equal p_3 "storage" then prim_3
            else raise (Parse_error "missing script storage")
          in
          let code = code_parse code in
          let param = param_parse param in
          let storage = storage_parse storage in
          Adt.{ param; storage; code }
      | n ->
          (* let error_msg =
               Stdlib.Format.sprintf "%s: syntax error@." (print_error_position l)
             in
             raise (Parse_error error_msg) *)
          raise_s
            (List.sexp_of_t (sexp_of_node Loc.sexp_of_t String.sexp_of_t) n))
  (* | Int (l, _) | String (l, _) | Bytes (l, _) | Prim (l, _, _, _) -> *)
  | n ->
      (* let error_msg =
           Stdlib.Format.sprintf "%s: syntax error@." (print_error_position l)
         in
         raise (Parse_error error_msg) *)
      raise_s (sexp_of_node Loc.sexp_of_t String.sexp_of_t n)
