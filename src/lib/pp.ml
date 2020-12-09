open Adt

let string_of_list f l =
  let open Printf in
  let values =
    let rec aux acc = function
      | [] -> ""
      | h :: tl ->
          aux
            ( if String.length acc > 0 then sprintf "%s; %s" acc (f h)
            else sprintf "%s" (f h) )
            tl
    in
    aux "" l
  in
  "[ " ^ values ^ " ]"

let rec string_of_data d =
  let open Printf in
  match d with
  | D_int d -> Z.to_string d
  | D_string s | D_bytes s -> sprintf "\"%s\"" s
  | D_elt (d_1, d_2) ->
      sprintf "Elt %s %s" (string_of_data d_1) (string_of_data d_2)
  | D_left d -> sprintf "Left %s" (string_of_data d)
  | D_right d -> sprintf "Right %s" (string_of_data d)
  | D_some d -> sprintf "Some %s" (string_of_data d)
  | D_none -> sprintf "None"
  | D_unit -> sprintf "Unit"
  | D_bool b -> sprintf (match b with true -> "True" | false -> "False")
  | D_pair (d_1, d_2) ->
      sprintf "(Pair %s %s)" (string_of_data d_1) (string_of_data d_2)
  | D_list d -> string_of_list string_of_data d

let rec string_of_typ t =
  let open Printf in
  match t with
  | T_int -> "int"
  | T_nat -> "nat"
  | T_string -> "string"
  | T_bytes -> "bytes"
  | T_mutez -> "mutez"
  | T_bool -> "bool"
  | T_key_hash -> "key_hash"
  | T_timestamp -> "timestamp"
  | T_address -> "address"
  | T_key -> "key"
  | T_unit -> "unit"
  | T_signature -> "signature"
  | T_option t -> sprintf "(option %s)" (string_of_typ t)
  | T_list t -> sprintf "(list %s)" (string_of_typ t)
  | T_set t -> sprintf "(set %s)" (string_of_typ t)
  | T_operation -> "operation"
  | T_contract t -> sprintf "(contract %s)" (string_of_typ t)
  | T_pair (t_1, t_2) ->
      sprintf "(pair %s %s)" (string_of_typ t_1) (string_of_typ t_2)
  | T_or (t_1, t_2) ->
      sprintf "(or %s %s)" (string_of_typ t_1) (string_of_typ t_2)
  | T_lambda (t_1, t_2) ->
      sprintf "(lambda %s %s)" (string_of_typ t_1) (string_of_typ t_2)
  | T_map (t_1, t_2) ->
      sprintf "(map %s %s)" (string_of_typ t_1) (string_of_typ t_2)
  | T_big_map (t_1, t_2) ->
      sprintf "(big_map %s %s)" (string_of_typ t_1) (string_of_typ t_2)
  | T_chain_id -> sprintf "chain_id"

let rec print_inst ch i =
  let open Printf in
  match i with
  | I_abs -> fprintf ch "ABS"
  | I_drop -> fprintf ch "DROP"
  | I_dup -> fprintf ch "DUP"
  | I_swap -> fprintf ch "SWAP"
  | I_some -> fprintf ch "SOME"
  | I_unit -> fprintf ch "UNIT"
  | I_pair -> fprintf ch "PAIR"
  | I_car -> fprintf ch "CAR"
  | I_cdr -> fprintf ch "CDR"
  | I_cons -> fprintf ch "CONS"
  | I_size -> fprintf ch "SIZE"
  | I_mem -> fprintf ch "MEM"
  | I_get -> fprintf ch "GET"
  | I_update -> fprintf ch "UPDATE"
  | I_exec -> fprintf ch "EXEC"
  | I_failwith -> fprintf ch "FAILWITH"
  | I_cast t -> fprintf ch "CAST %s" (string_of_typ t)
  | I_rename -> fprintf ch "RENAME"
  | I_concat -> fprintf ch "CONCAT"
  | I_slice -> fprintf ch "SLICE"
  | I_pack -> fprintf ch "PACK"
  | I_add -> fprintf ch "ADD"
  | I_sub -> fprintf ch "SUB"
  | I_mul -> fprintf ch "MUL"
  | I_ediv -> fprintf ch "EDIV"
  | I_isnat -> fprintf ch "ISNAT"
  | I_int -> fprintf ch "INT"
  | I_neg -> fprintf ch "NEG"
  | I_lsl -> fprintf ch "LSL"
  | I_lsr -> fprintf ch "LSR"
  | I_or -> fprintf ch "OR"
  | I_and -> fprintf ch "AND"
  | I_xor -> fprintf ch "XOR"
  | I_not -> fprintf ch "NOT"
  | I_compare -> fprintf ch "COMPARE"
  | I_eq -> fprintf ch "EQ"
  | I_neq -> fprintf ch "NEQ"
  | I_lt -> fprintf ch "LT"
  | I_gt -> fprintf ch "GT"
  | I_le -> fprintf ch "LE"
  | I_ge -> fprintf ch "GE"
  | I_self -> fprintf ch "SELF"
  | I_transfer_tokens -> fprintf ch "TRANSFER_TOKENS"
  | I_set_delegate -> fprintf ch "SET_DELEGATE"
  | I_implicit_account -> fprintf ch "IMPLICIT_ACCOUNT"
  | I_now -> fprintf ch "NOW"
  | I_amount -> fprintf ch "AMOUNT"
  | I_balance -> fprintf ch "BALANCE"
  | I_check_signature -> fprintf ch "CHECK_SIGNATURE"
  | I_blake2b -> fprintf ch "BLAKE2B"
  | I_sha256 -> fprintf ch "SHA256"
  | I_sha512 -> fprintf ch "SHA512"
  | I_hash_key -> fprintf ch "HASH_KEY"
  | I_source -> fprintf ch "SOURCE"
  | I_sender -> fprintf ch "SENDER"
  | I_address -> fprintf ch "ADDRESS"
  | I_chain_id -> fprintf ch "CHAIN_ID"
  | I_noop -> fprintf ch ""
  | I_unpair -> fprintf ch "UNPAIR"
  | I_seq (i_1, i_2) -> fprintf ch "%a; %a" print_inst i_1 print_inst i_2
  | I_drop_n n when n = Z.one -> fprintf ch "DROP"
  | I_drop_n n -> fprintf ch "DROP %s" (Z.to_string n)
  | I_dig n -> fprintf ch "DIG %s" (Z.to_string n)
  | I_dug n -> fprintf ch "DUG %s" (Z.to_string n)
  | I_push (t, d) ->
      fprintf ch "PUSH %s %s" (string_of_typ t) (string_of_data d)
  | I_none t -> fprintf ch "NONE %s" (string_of_typ t)
  | I_if_none (i_1, i_2) ->
      fprintf ch "IF_NONE { %a } { %a }" print_inst i_1 print_inst i_2
  | I_if_some (i_1, i_2) ->
      fprintf ch "IF_SOME { %a } { %a }" print_inst i_1 print_inst i_2
  | I_left t -> fprintf ch "LEFT %s" (string_of_typ t)
  | I_right t -> fprintf ch "RIGHT %s" (string_of_typ t)
  | I_if_left (i_1, i_2) ->
      fprintf ch "IF_LEFT { %a } { %a }" print_inst i_1 print_inst i_2
  | I_if_right (i_1, i_2) ->
      fprintf ch "IF_RIGHT { %a } { %a }" print_inst i_1 print_inst i_2
  | I_nil t -> fprintf ch "NIL %s" (string_of_typ t)
  | I_if_cons (i_1, i_2) ->
      fprintf ch "IF_CONS { %a } { %a }" print_inst i_1 print_inst i_2
  | I_empty_set t -> fprintf ch "EMPTY_SET %s" (string_of_typ t)
  | I_empty_map (t_1, t_2) ->
      fprintf ch "EMPTY_MAP %s %s" (string_of_typ t_1) (string_of_typ t_2)
  | I_empty_big_map (t_1, t_2) ->
      fprintf ch "EMPTY_BIG_MAP %s %s" (string_of_typ t_1) (string_of_typ t_2)
  | I_map i -> fprintf ch "MAP { %a }" print_inst i
  | I_iter i -> fprintf ch "ITER { %a }" print_inst i
  | I_if (i_1, i_2) ->
      fprintf ch "IF { %a } { %a }" print_inst i_1 print_inst i_2
  | I_loop i -> fprintf ch "LOOP { %a }" print_inst i
  | I_loop_left i -> fprintf ch "LOOP_LEFT { %a }" print_inst i
  | I_lambda (t_1, t_2, i) ->
      fprintf ch "LAMBDA %s %s { %a }" (string_of_typ t_1) (string_of_typ t_2)
        print_inst i
  | I_dip i -> fprintf ch "DIP { %a }" print_inst i
  | I_dip_n (n, i) -> fprintf ch "DIP %s { %a }" (Z.to_string n) print_inst i
  | I_unpack t -> fprintf ch "UNPACK %s" (string_of_typ t)
  | I_contract t -> fprintf ch "CONTRACT %s" (string_of_typ t)
  | I_create_contract p -> fprintf ch "CREATE_CONTRACT { %a }" program p

and program fmt { code; param; storage } =
  let open Printf in
  let () = fprintf fmt "parameter %s;\n" (string_of_typ param) in
  let () = fprintf fmt "storage %s;\n" (string_of_typ storage) in
  fprintf fmt "code { %a }\n" print_inst code
