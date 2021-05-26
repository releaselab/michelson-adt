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
  let res =
    try Ok (Parser.start Lexer.next_token lexbuf) with
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
  | Prim (loc, t, args, annots) ->
      let value =
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
        | _ ->
            let error_msg =
              Stdlib.Format.sprintf "%s: ill-formed type@."
                (print_error_position loc)
            in
            raise (Parse_error error_msg)
      in
      let annots = List.filter_map annots ~f:annot_of_string in
      { value; loc; annots }
  | Int (l, _) | String (l, _) | Bytes (l, _) | Seq (l, _, _) ->
      let error_msg =
        Stdlib.Format.sprintf "%s: syntax error@." (print_error_position l)
      in
      raise (Parse_error error_msg)

let rec data_parse =
  let annots = [] in
  function
  | Int (loc, n) -> { value = D_int n; loc; annots }
  | Bytes (loc, b) -> { value = D_bytes b; loc; annots }
  | String (loc, s) -> { value = D_string s; loc; annots }
  | Prim (loc, p, args, annots) ->
      let value =
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
                (print_error_position loc)
            in
            raise (Parse_error error_msg)
      in
      let annots = List.filter_map annots ~f:annot_of_string in
      { value; loc; annots }
  | Seq (loc, datas, _) -> (
      try { value = D_list (List.map datas ~f:data_parse); loc; annots }
      with Parse_error _ ->
        {
          value =
            D_instruction
              { value = I_seq (List.map datas ~f:inst_parse); loc; annots };
          loc;
          annots;
        })

and inst_parse = function
  | Seq (loc, s, _) ->
      let value = I_seq (List.map s ~f:inst_parse) in
      { value; loc; annots = [] }
  | Prim (loc, i, args, annots) ->
      let value =
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
        | "CREATE_CONTRACT", [ p ] ->
            let c, _ = program_parse p in
            I_create_contract c
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
        | "CAST", [ t ] -> I_cast (type_parse t)
        | "RENAME", [] -> I_rename
        | _ ->
            let error_msg =
              Stdlib.Format.sprintf "%s: unknown primitive@."
                (print_error_position loc)
            in
            raise (Parse_error error_msg)
      in
      let annots = List.filter_map annots ~f:annot_of_string in
      { value; loc; annots }
  | Int (l, _) | String (l, _) | Bytes (l, _) ->
      let error_msg =
        Stdlib.Format.sprintf "%s: syntax error@." (print_error_position l)
      in
      raise (Parse_error error_msg)

and code_parse l = function
  | [ Seq (loc, insts, _) ] ->
      { value = I_seq (List.map insts ~f:inst_parse); loc; annots = [] }
  | Seq (_, _, _) :: _ :: _
  | (Int (_, _) | String (_, _) | Bytes (_, _) | Prim (_, _, _, _)) :: _
  | [] ->
      let error_msg =
        Stdlib.Format.sprintf "%s: code of contract must be a sequence@."
          (print_error_position l)
      in
      raise (Parse_error error_msg)

and param_parse l = function
  | [ (Prim _ as t) ] -> type_parse t
  | Prim (_, _, _, _) :: _ :: _
  | (Int (_, _) | String (_, _) | Bytes (_, _) | Seq (_, _, _)) :: _
  | [] ->
      let error_msg =
        Stdlib.Format.sprintf "%s: syntax error@." (print_error_position l)
      in
      raise (Parse_error error_msg)

and storage_parse l = function
  | [ (Prim _ as t) ] -> type_parse t
  | Prim (_, _, _, _) :: _ :: _
  | (Int (_, _) | String (_, _) | Bytes (_, _) | Seq (_, _, _)) :: _
  | [] ->
      let error_msg =
        Stdlib.Format.sprintf "%s: syntax error@." (print_error_position l)
      in
      raise (Parse_error error_msg)

and program_parse = function
  | Seq (_l, nodes, spec) -> (
      match nodes with
      | [
       Prim (l_1, p_1, a_1, _); Prim (l_2, p_2, a_2, _); Prim (l_3, p_3, a_3, _);
      ] ->
          let l_code, code =
            if String.equal p_1 "code" then (l_1, a_1)
            else if String.equal p_2 "code" then (l_2, a_2)
            else if String.equal p_3 "code" then (l_3, a_3)
            else raise (Parse_error "missing script code")
          in
          let l_param, param =
            if String.equal p_1 "parameter" then (l_1, a_1)
            else if String.equal p_2 "parameter" then (l_2, a_2)
            else if String.equal p_3 "parameter" then (l_3, a_3)
            else raise (Parse_error "missing script parameter")
          in
          let l_storage, storage =
            if String.equal p_1 "storage" then (l_1, a_1)
            else if String.equal p_2 "storage" then (l_2, a_2)
            else if String.equal p_3 "storage" then (l_3, a_3)
            else raise (Parse_error "missing script storage")
          in
          let code = code_parse l_code code in
          let param = param_parse l_param param in
          let storage = storage_parse l_storage storage in
          (Adt.{ param; storage; code }, spec)
      | n ->
          (* let error_msg =
               Stdlib.Format.sprintf "%s: syntax error@." (print_error_position l)
             in
             raise (Parse_error error_msg) *)
          raise_s
            (List.sexp_of_t
               (Micheline.Node.sexp_of_node Loc.sexp_of_t String.sexp_of_t)
               n))
  (* | Int (l, _) | String (l, _) | Bytes (l, _) | Prim (l, _, _, _) -> *)
  | n ->
      (* let error_msg =
           Stdlib.Format.sprintf "%s: syntax error@." (print_error_position l)
         in
         raise (Parse_error error_msg) *)
      raise_s (Micheline.Node.sexp_of_node Loc.sexp_of_t String.sexp_of_t n)
