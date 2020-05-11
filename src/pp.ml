open Adt
open Format

let rec data fmt = function
  | D_int d -> Z.pp_print fmt d
  | D_string s | D_bytes s -> fprintf fmt "\"%s\"" s
  | D_elt (d_1, d_2) -> fprintf fmt "Elt %a %a" data d_1 data d_2
  | D_left d -> fprintf fmt "Left %a" data d
  | D_right d -> fprintf fmt "Right %a" data d
  | D_some d -> fprintf fmt "Some %a" data d
  | D_none -> pp_print_string fmt "None"
  | D_unit -> fprintf fmt "Unit"
  | D_bool b -> fprintf fmt (match b with true -> "True" | false -> "False")
  | D_pair (d_1, d_2) -> fprintf fmt "Pair %a %a" data d_1 data d_2
  | D_list d ->
      let () = pp_print_string fmt "{" in
      let () =
        pp_print_list
          ~pp_sep:(fun fmt () -> pp_print_string fmt "; ")
          data fmt d
      in
      pp_print_string fmt "}"

let simple_comparable_type fmt t =
  pp_print_string fmt
    ( match t with
    | T_int -> "int"
    | T_nat -> "nat"
    | T_string -> "string"
    | T_bytes -> "bytes"
    | T_mutez -> "mutez"
    | T_bool -> "bool"
    | T_key_hash -> "key_hash"
    | T_timestamp -> "timestamp"
    | T_address -> "address" )

let rec comparable_type fmt = function
  | T_simple_comparable_type t -> simple_comparable_type fmt t
  | T_comparable_pair ((t_1, _), (t_2, _)) ->
      fprintf fmt "(pair %a %a)" simple_comparable_type t_1 comparable_type t_2

let rec typ fmt =
  let print_string = pp_print_string fmt in
  function
  | T_comparable t -> comparable_type fmt t
  | T_key -> print_string "key"
  | T_unit -> print_string "unit"
  | T_signature -> print_string "signature"
  | T_option (t, _) -> fprintf fmt "(option %a)" typ t
  | T_list (t, _) -> fprintf fmt "(list %a)" typ t
  | T_set (t, _) -> fprintf fmt "(set %a)" comparable_type t
  | T_operation -> print_string "operation"
  | T_contract (t, _) -> fprintf fmt "(contract %a)" typ t
  | T_pair ((t_1, _), (t_2, _)) -> fprintf fmt "(pair %a %a)" typ t_1 typ t_2
  | T_or ((t_1, _), (t_2, _)) -> fprintf fmt "(or %a %a)" typ t_1 typ t_2
  | T_lambda ((t_1, _), (t_2, _)) ->
      fprintf fmt "(lambda %a %a)" typ t_1 typ t_2
  | T_map ((t_1, _), (t_2, _)) ->
      fprintf fmt "(map %a %a)" comparable_type t_1 typ t_2
  | T_big_map ((t_1, _), (t_2, _)) ->
      fprintf fmt "(big_map %a %a)" comparable_type t_1 typ t_2
  | T_chain_id -> print_string "chain_id"

let rec inst fmt =
  let print_string = pp_print_string fmt in
  function
  | I_abs -> print_string "ABS"
  | I_drop -> print_string "DROP"
  | I_dup -> print_string "DUP"
  | I_swap -> print_string "SWAP"
  | I_some -> print_string "SOME"
  | I_unit -> print_string "UNIT"
  | I_pair -> print_string "PAIR"
  | I_car -> print_string "CAR"
  | I_cdr -> print_string "CDR"
  | I_cons -> print_string "CONS"
  | I_size -> print_string "SIZE"
  | I_mem -> print_string "MEM"
  | I_get -> print_string "GET"
  | I_update -> print_string "UPDATE"
  | I_exec -> print_string "EXEC"
  | I_failwith -> print_string "FAILWITH"
  | I_cast -> print_string "CAST"
  | I_rename -> print_string "RENAME"
  | I_concat -> print_string "CONCAT"
  | I_slice -> print_string "SLICE"
  | I_pack -> print_string "PACK"
  | I_add -> print_string "ADD"
  | I_sub -> print_string "SUB"
  | I_mul -> print_string "MUL"
  | I_ediv -> print_string "EDIV"
  | I_isnat -> print_string "ISNAT"
  | I_int -> print_string "INT"
  | I_neg -> print_string "NEG"
  | I_lsl -> print_string "LSL"
  | I_lsr -> print_string "LSR"
  | I_or -> print_string "OR"
  | I_and -> print_string "AND"
  | I_xor -> print_string "XOR"
  | I_not -> print_string "NOT"
  | I_compare -> print_string "COMPARE"
  | I_eq -> print_string "EQ"
  | I_neq -> print_string "NEQ"
  | I_lt -> print_string "LT"
  | I_gt -> print_string "GT"
  | I_le -> print_string "LE"
  | I_ge -> print_string "GE"
  | I_self -> print_string "SELF"
  | I_transfer_tokens -> print_string "TRANSFER_TOKENS"
  | I_set_delegate -> print_string "SET_DELEGATE"
  | I_create_account -> print_string "CREATE_ACCOUNT"
  | I_implicit_account -> print_string "IMPLICIT_ACCOUNT"
  | I_now -> print_string "NOW"
  | I_amount -> print_string "AMOUNT"
  | I_balance -> print_string "BALANCE"
  | I_check_signature -> print_string "CHECK_SIGNATURE"
  | I_blake2b -> print_string "BLAKE2B"
  | I_sha256 -> print_string "SHA256"
  | I_sha512 -> print_string "SHA512"
  | I_hash_key -> print_string "HASH_KEY"
  | I_steps_to_quota -> print_string "STEPS_TO_QUOTA"
  | I_source -> print_string "SOURCE"
  | I_sender -> print_string "SENDER"
  | I_address -> print_string "ADDRESS"
  | I_chain_id -> print_string "CHAIN_ID"
  | I_noop -> print_string ""
  | I_unpair -> print_string "UNPAIR"
  | I_seq ((i_1, _), (i_2, _)) -> fprintf fmt "%a; %a" inst i_1 inst i_2
  | I_drop_n n when n = Z.one -> print_string "DROP"
  | I_drop_n n -> fprintf fmt "DROP %a" Z.pp_print n
  | I_dig n -> fprintf fmt "DIG %a" Z.pp_print n
  | I_dug n -> fprintf fmt "DUG %a" Z.pp_print n
  | I_push ((t, _), d) -> fprintf fmt "PUSH %a %a" typ t data d
  | I_none (t, _) -> fprintf fmt "NONE %a" typ t
  | I_if_none ((i_1, _), (i_2, _)) ->
      fprintf fmt "IF_NONE { %a } { %a }" inst i_1 inst i_2
  | I_left (t, _) -> fprintf fmt "LEFT %a" typ t
  | I_right (t, _) -> fprintf fmt "RIGHT %a" typ t
  | I_if_left ((i_1, _), (i_2, _)) ->
      fprintf fmt "IF_LEFT { %a } { %a }" inst i_1 inst i_2
  | I_if_right ((i_1, _), (i_2, _)) ->
      fprintf fmt "IF_RIGHT { %a } { %a }" inst i_1 inst i_2
  | I_nil (t, _) -> fprintf fmt "NIL %a" typ t
  | I_if_cons ((i_1, _), (i_2, _)) ->
      fprintf fmt "IF_CONS { %a } { %a }" inst i_1 inst i_2
  | I_empty_set (t, _) -> fprintf fmt "EMPTY_SET %a" comparable_type t
  | I_empty_map ((t_1, _), (t_2, _)) ->
      fprintf fmt "EMPTY_MAP %a %a" comparable_type t_1 typ t_2
  | I_empty_big_map ((t_1, _), (t_2, _)) ->
      fprintf fmt "EMPTY_BIG_MAP %a %a" comparable_type t_1 typ t_2
  | I_map (i, _) -> fprintf fmt "MAP { %a }" inst i
  | I_iter (i, _) -> fprintf fmt "ITER { %a }" inst i
  | I_if ((i_1, _), (i_2, _)) ->
      fprintf fmt "IF { %a } { %a }" inst i_1 inst i_2
  | I_loop (i, _) -> fprintf fmt "LOOP { %a }" inst i
  | I_loop_left (i, _) -> fprintf fmt "LOOP_LEFT { %a }" inst i
  | I_lambda ((t_1, _), (t_2, _), (i, _)) ->
      fprintf fmt "LAMBDA %a %a { %a }" typ t_1 typ t_2 inst i
  | I_dip (i, _) -> fprintf fmt "DIP { %a }" inst i
  | I_dip_n (n, (i, _)) -> fprintf fmt "DIP %a { %a }" Z.pp_print n inst i
  | I_unpack (t, _) -> fprintf fmt "UNPACK %a" typ t
  | I_contract (t, _) -> fprintf fmt "CONTRACT %a" typ t
  | I_create_contract p -> fprintf fmt "CREATE_CONTRACT { %a }" program p

and program fmt { code; param; storage } =
  let () = fprintf fmt "parameter %a;\n" typ (fst param) in
  let () = fprintf fmt "storage %a;\n" typ (fst storage) in
  fprintf fmt "code { %a }\n" inst code
