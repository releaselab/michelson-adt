open Tezos_micheline
open Adt

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  let () = close_in ch in
  s

let parse_file filename =
  let s = read_file filename in
  match Micheline_parser.tokenize s with
  | _, _ :: _ -> failwith "Cannot tokenize"
  | tokens, [] -> (
      match Micheline_parser.parse_toplevel tokens with
      | _, _ :: _ -> failwith "Cannot parse_toplevel"
      | ast, [] -> ast )

let error t =
  let loc = Micheline.location t in
  failwith
    (Printf.sprintf "ill-formed code: line %d, col %d to line %d, col %d"
       loc.Micheline_parser.start.line
       (loc.Micheline_parser.start.column + 1)
       loc.Micheline_parser.stop.line
       (loc.Micheline_parser.stop.column + 1))

let token_location token =
  let open Micheline in
  let loc = location token in
  let open Micheline_parser in
  let open Location in
  let s = { lin = loc.start.line; col = loc.start.column + 1 } in
  let e = { lin = loc.stop.line; col = loc.stop.column + 1 } in
  { s; e }

let get_annot a =
  let a' = String.sub a 1 (String.length a - 1) in
  match a.[0] with
  | ':' -> A_type a'
  | '@' -> A_var a'
  | '%' -> A_field a'
  | _ -> assert false

let rec typ token =
  let t =
    let open Micheline in
    match token with
    | Prim (_, "unit", [], _) -> T_unit
    | Prim (_, "bool", [], _) -> T_bool
    | Prim (_, "int", [], _) -> T_int
    | Prim (_, "nat", [], _) -> T_nat
    | Prim (_, "string", [], _) -> T_string
    | Prim (_, "chain_id", [], _) -> T_chain_id
    | Prim (_, "bytes", [], _) -> T_bytes
    | Prim (_, "mutez", [], _) -> T_mutez
    | Prim (_, "key_hash", [], _) -> T_key_hash
    | Prim (_, "key", [], _) -> T_key
    | Prim (_, "signature", [], _) -> T_signature
    | Prim (_, "timestamp", [], _) -> T_timestamp
    | Prim (_, "address", [], _) -> T_address
    | Prim (_, "option", [ t ], _) -> T_option (typ t)
    | Prim (_, "or", [ t_1; t_2 ], _) -> T_or (typ t_1, typ t_2)
    | Prim (_, "pair", [ t_1; t_2 ], _) -> T_pair (typ t_1, typ t_2)
    | Prim (_, "list", [ t ], _) -> T_list (typ t)
    | Prim (_, "set", [ t ], _) -> T_set (typ t)
    | Prim (_, "operation", [], _) -> T_operation
    | Prim (_, "contract", [ t ], _) -> T_contract (typ t)
    | Prim (_, "lambda", [ t_1; t_2 ], _) -> T_lambda (typ t_1, typ t_2)
    | Prim (_, "big_map", [ t_1; t_2 ], _) -> T_big_map (typ t_1, typ t_2)
    | Prim (_, "map", [ t_1; t_2 ], _) -> T_map (typ t_1, typ t_2)
    | _ -> error token
  in
  match token with
  | Prim (_, _, _, l) -> (token_location token, t, List.map get_annot l)
  | _ -> assert false

let rec data token =
  let t =
    let open Micheline in
    match token with
    | Int (_, n) -> D_int n
    | String (_, s) -> D_string s
    | Bytes (_, b) -> D_bytes b
    | Prim (_, "Unit", [], _) -> D_unit
    | Prim (_, "True", [], _) -> D_bool true
    | Prim (_, "False", [], _) -> D_bool false
    | Prim (_, "Pair", [ d_1; d_2 ], _) -> D_pair (data d_1, data d_2)
    | Prim (_, "Left", [ d ], _) -> D_left (data d)
    | Prim (_, "Right", [ d ], _) -> D_right (data d)
    | Prim (_, "Some", [ d ], _) -> D_some (data d)
    | Prim (_, "None", [], _) -> D_none
    | Prim (_, "Elt", [ d_1; d_2 ], _) -> D_elt (data d_1, data d_2)
    | Prim _ as i -> D_instruction (inst i)
    | Seq (_, l) ->
        let l_d = List.map data l in
        if List.for_all (function _, D_instruction _ -> true | _ -> false) l_d
        then
          let l_i =
            List.map
              (function _, D_instruction i -> i | _ -> assert false)
              l_d
          in
          D_instruction (token_location token, I_seq l_i, [])
        else D_list l_d
  in
  (token_location token, t)

and inst token =
  let t =
    let open Micheline in
    match token with
    | Seq (_, l) -> I_seq (List.map inst l)
    | Prim (_, "RENAME", [], _) -> I_rename
    | Prim (_, "FAILWITH", [], _) -> I_failwith
    | Prim (_, "IF", [ i_t; i_f ], _) -> I_if (inst i_t, inst i_f)
    | Prim (_, "LOOP", [ b ], _) -> I_loop (inst b)
    | Prim (_, "LOOP_LEFT", [ b ], _) -> I_loop_left (inst b)
    | Prim (_, "DIP", [ b ], _) -> I_dip (inst b)
    | Prim (_, "DIP", [ Int (_, n); b ], _) -> I_dip_n (n, inst b)
    | Prim (_, "EXEC", [], _) -> I_exec
    | Prim (_, "APPLY", [], _) -> I_apply
    | Prim (_, "DROP", [], _) -> I_drop
    | Prim (_, "DROP", [ Int (_, n) ], _) -> I_drop_n n
    | Prim (_, "DUP", [], _) -> I_dup
    | Prim (_, "SWAP", [], _) -> I_swap
    | Prim (_, "DIG", [ Int (_, n) ], _) -> I_dig n
    | Prim (_, "DUG", [ Int (_, n) ], _) -> I_dug n
    | Prim (_, "PUSH", [ t; d ], _) -> I_push (typ t, data d)
    | Prim (_, "UNIT", [], _) -> I_unit
    | Prim (_, "LAMBDA", [ t_1; t_2; i ], _) ->
        I_lambda (typ t_1, typ t_2, inst i)
    | Prim (_, "EQ", [], _) -> I_eq
    | Prim (_, "NEQ", [], _) -> I_neq
    | Prim (_, "LT", [], _) -> I_lt
    | Prim (_, "GT", [], _) -> I_gt
    | Prim (_, "LE", [], _) -> I_le
    | Prim (_, "GE", [], _) -> I_ge
    | Prim (_, "OR", [], _) -> I_or
    | Prim (_, "AND", [], _) -> I_and
    | Prim (_, "XOR", [], _) -> I_xor
    | Prim (_, "NOT", [], _) -> I_not
    | Prim (_, "NEG", [], _) -> I_neg
    | Prim (_, "ABS", [], _) -> I_abs
    | Prim (_, "ISNAT", [], _) -> I_isnat
    | Prim (_, "INT", [], _) -> I_int
    | Prim (_, "ADD", [], _) -> I_add
    | Prim (_, "SUB", [], _) -> I_sub
    | Prim (_, "MUL", [], _) -> I_mul
    | Prim (_, "EDIV", [], _) -> I_ediv
    | Prim (_, "LSL", [], _) -> I_lsl
    | Prim (_, "LSR", [], _) -> I_lsr
    | Prim (_, "COMPARE", [], _) -> I_compare
    | Prim (_, "CONCAT", [], _) -> I_concat
    | Prim (_, "SIZE", [], _) -> I_size
    | Prim (_, "SLICE", [], _) -> I_slice
    | Prim (_, "PAIR", [], _) -> I_pair
    | Prim (_, "CAR", [], _) -> I_car
    | Prim (_, "CDR", [], _) -> I_cdr
    | Prim (_, "EMPTY_SET", [ t ], _) -> I_empty_set (typ t)
    | Prim (_, "MEM", [], _) -> I_mem
    | Prim (_, "UPDATE", [], _) -> I_update
    | Prim (_, "ITER", [ i ], _) -> I_iter (inst i)
    | Prim (_, "EMPTY_MAP", [ t_1; t_2 ], _) -> I_empty_map (typ t_1, typ t_2)
    | Prim (_, "GET", [], _) -> I_get
    | Prim (_, "MAP", [ i ], _) -> I_map (inst i)
    | Prim (_, "EMPTY_BIG_MAP", [ t_1; t_2 ], _) ->
        I_empty_big_map (typ t_1, typ t_2)
    | Prim (_, "SOME", [], _) -> I_some
    | Prim (_, "NONE", [ t ], _) -> I_none (typ t)
    | Prim (_, "IF_NONE", [ i_1; i_2 ], _) -> I_if_none (inst i_1, inst i_2)
    | Prim (_, "LEFT", [ t ], _) -> I_left (typ t)
    | Prim (_, "RIGHT", [ t ], _) -> I_right (typ t)
    | Prim (_, "IF_LEFT", [ i_1; i_2 ], _) -> I_if_left (inst i_1, inst i_2)
    | Prim (_, "CONS", [], _) -> I_cons
    | Prim (_, "NIL", [ t ], _) -> I_nil (typ t)
    | Prim (_, "IF_CONS", [ i_1; i_2 ], _) -> I_if_cons (inst i_1, inst i_2)
    | Prim (_, "CREATE_CONTRACT", [ Seq (_, p) ], _) ->
        I_create_contract (convert p)
    | Prim (_, "TRANSFER_TOKENS", [], _) -> I_transfer_tokens
    | Prim (_, "SET_DELEGATE", [], _) -> I_set_delegate
    | Prim (_, "BALANCE", [], _) -> I_balance
    | Prim (_, "ADDRESS", [], _) -> I_address
    | Prim (_, "CONTRACT", [ t ], _) -> I_contract (typ t)
    | Prim (_, "SOURCE", [], _) -> I_source
    | Prim (_, "SENDER", [], _) -> I_sender
    | Prim (_, "SELF", [], _) -> I_self
    | Prim (_, "AMOUNT", [], _) -> I_amount
    | Prim (_, "IMPLICIT_ACCOUNT", [], _) -> I_implicit_account
    | Prim (_, "NOW", [], _) -> I_now
    | Prim (_, "CHAIN_ID", [], _) -> I_chain_id
    | Prim (_, "PACK", [], _) -> I_pack
    | Prim (_, "UNPACK", [ t ], _) -> I_unpack (typ t)
    | Prim (_, "HASH_KEY", [], _) -> I_hash_key
    | Prim (_, "BLAKE2B", [], _) -> I_blake2b
    | Prim (_, "SHA256", [], _) -> I_sha256
    | Prim (_, "SHA512", [], _) -> I_sha512
    | Prim (_, "CHECK_SIGNATURE", [], _) -> I_check_signature
    | Prim (_, "CAST", [ t ], _) -> I_cast (typ t)
    | Prim (_, "UNPAIR", [], _) -> I_unpair
    | t -> error t
  in
  match token with
  | Seq _ -> (token_location token, t, [])
  | Prim (_, _, _, l) -> (token_location token, t, List.map get_annot l)
  | _ -> assert false

and convert ast =
  let parameter =
    List.find_map
      (function
        | Micheline.Prim (_, "parameter", [ t ], _) -> Some t | _ -> None)
      ast
  in
  let param = typ (Option.get parameter) in
  let storage =
    List.find_map
      (function Micheline.Prim (_, "storage", [ t ], _) -> Some t | _ -> None)
      ast
  in
  let storage = typ (Option.get storage) in
  let code =
    List.find_map
      (function Micheline.Prim (_, "code", [ c ], _) -> Some c | _ -> None)
      ast
  in
  let code = inst (Option.get code) in
  { code; param; storage }
