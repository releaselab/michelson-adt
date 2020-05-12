
%{
  open Adt

  (*open Lexing*)

  (*let func_num = ref 0*)

  (*let new_func _ = func_num := !func_num + 1*)

  (*let var x = x ^ "_" ^ string_of_int(!func_num)*)

  (*let position_to_string (s,e) =
    Printf.sprintf "%s:%d:%d-%d" s.pos_fname s.pos_lnum (s.pos_cnum - s.pos_bol + 1) (e.pos_cnum - e.pos_bol + 1)*)

  (*let print_error msg pos =
    Printf.fprintf stderr "%s: %s\n" (position_to_string pos) msg; exit(-1)*)
    
    (* let i = ref (-1)
    let label () =
      let () = i := !i + 1 in
      !i *)
  
  (*let instruction_list_to_seq =
    List.fold_left (fun acc i -> create_node ~loc:acc.loc (Inst (I_seq (acc, i)))) (create_node (Inst I_noop))*)

  let create_pair t_1 t_2 =
    match t_1, t_2 with
        (T_comparable (T_simple_comparable_type t_1), a_1), (T_comparable t_2, a_2) ->
          T_comparable (T_comparable_pair ((t_1, a_1), (t_2, a_2)))
      | _ -> T_pair (t_1, t_2)
%}

%token <Z.t> NUM
%token <bool> BOOLEAN
/*%token <string> IDENT*/
%token <string> HEX
%token <string> STRING
%token <string> IDENT
%token LB RB LP RP
%token SEMICOLON MINUS PERCENT AT COLON
%token PARAMETER STORAGE CODE UNIT PAIR LEFT RIGHT SOME NONE ELT I_DROP
%token I_DUP I_SWAP I_DIG I_DUG I_PUSH I_SOME I_NONE I_UNIT I_IF_NONE I_PAIR
%token I_CAR I_CDR I_LEFT I_RIGHT I_IF_LEFT I_NIL I_CONS I_IF_CONS I_SIZE 
%token I_EMPTY_SET I_EMPTY_MAP I_EMPTY_BIG_MAP I_MAP I_ITER I_MEM I_GET I_UPDATE
%token I_IF I_LOOP I_LOOP_LEFT I_LAMBDA I_EXEC I_DIP I_FAILWITH I_CAST I_RENAME
%token I_CONCAT I_SLICE I_PACK I_UNPACK I_ADD I_SUB I_MUL I_EDIV I_ABS I_ISNAT
%token I_INT I_NEG I_LSL I_LSR I_OR I_AND I_XOR I_NOT I_COMPARE I_EQ I_NEQ I_LT
%token I_GT I_LE I_GE I_SELF I_CONTRACT I_TRANSFER_TOKENS I_SET_DELEGATE
%token I_CREATE_ACCOUNT I_CREATE_CONTRACT I_IMPLICIT_ACCOUNT I_NOW I_AMOUNT
%token I_BALANCE I_CHECK_SIGNATURE I_BLAKE2B I_SHA256 I_SHA512 I_HASH_KEY
%token I_STEPS_TO_QUOTA I_SOURCE I_SENDER I_ADDRESS I_CHAIN_ID I_UNPAIR T_KEY
%token T_UNIT T_SIGNATURE T_OPTION T_LIST T_SET T_OPERATION T_CONTRACT T_PAIR
%token T_OR T_LAMBDA T_MAP T_BIG_MAP T_CHAIN_ID T_INT T_NAT T_STRING T_BYTES
%token T_MUTEZ T_BOOL T_KEY_HASH T_TIMESTAMP T_ADDRESS I_IF_SOME
%token EOF

%start <Adt.program> start

%%

start:
  p=program EOF { p }

program:
    CODE code=code SEMICOLON STORAGE storage=storage SEMICOLON PARAMETER param=parameter SEMICOLON?
  | CODE code=code SEMICOLON PARAMETER param=parameter SEMICOLON STORAGE storage=storage SEMICOLON?
  | STORAGE storage=storage SEMICOLON CODE code=code SEMICOLON PARAMETER param=parameter SEMICOLON?
  | STORAGE storage=storage SEMICOLON PARAMETER param=parameter SEMICOLON CODE code=code SEMICOLON?
  | PARAMETER param=parameter SEMICOLON CODE code=code SEMICOLON STORAGE storage=storage SEMICOLON?
  | PARAMETER param=parameter SEMICOLON STORAGE storage=storage SEMICOLON CODE code=code SEMICOLON?  { { param; storage; code; } }

code:
    i=instruction_block { fst i }

storage:
    t=typ { t }

parameter:
    t=typ { t }

%inline field_annot:
    PERCENT i=IDENT { i }

%inline type_annot:
    COLON i=IDENT { i }

%inline type_annotated(T):
    T                     { None }
  | LP T a=type_annot? RP { a }

typ:
    ta=comparable_type                                        { (T_comparable (fst ta), snd ta) }
  | a=type_annotated(T_KEY)                                   { (T_key, a) }
  | a=type_annotated(T_UNIT)                                  { (T_unit, a) }
  | a=type_annotated(T_SIGNATURE)                             { (T_signature, a) }
  | a=type_annotated(T_OPERATION)                             { (T_operation, a) }
  | a=type_annotated(T_CHAIN_ID)                              { (T_chain_id, a) }
  | LP T_OPTION a=type_annot? t=typ RP                        { (T_option t, a) }
  | LP T_LIST a=type_annot? t=typ RP                          { (T_list t, a) }
  | LP T_SET a=type_annot? t=comparable_type RP               { (T_set t, a) }
  | LP T_CONTRACT a=type_annot? t=typ RP                      { (T_contract t, a) }
  | LP T_PAIR a=type_annot? t_1=typ t_2=typ RP                { (create_pair t_1 t_2, a) }
  | LP T_OR a=type_annot? t_1=typ t_2=typ RP                  { (T_or (t_1, t_2), a) }
  | LP T_LAMBDA a=type_annot? t_1=typ t_2=typ RP              { (T_lambda (t_1, t_2), a) }
  | LP T_MAP a=type_annot? t_1=comparable_type t_2=typ RP     { (T_map (t_1, t_2), a) }
  | LP T_BIG_MAP a=type_annot? t_1=comparable_type t_2=typ RP { (T_big_map (t_1, t_2), a) }

%inline simple_comparable_type:
    T_INT       { T_int }
  | T_NAT       { T_nat }
  | T_STRING    { T_string }
  | T_BYTES     { T_bytes }
  | T_MUTEZ     { T_mutez }
  | T_BOOL      { T_bool }
  | T_KEY_HASH  { T_key_hash }
  | T_TIMESTAMP { T_timestamp }
  | T_ADDRESS   { T_address }

%inline simple_comparable_type_annot:
    t=simple_comparable_type                      { t, None }
  | LP t=simple_comparable_type a=type_annot? RP  { t, a }

comparable_type:
    t=simple_comparable_type_annot { T_simple_comparable_type (fst t), (snd t) }

%inline instruction_block:
  LB i=instruction RB { i }

instruction:
    { (I_noop, []) }
  | i=instruction_d { i }
  | i_1=instruction_d SEMICOLON i_2=instruction { (I_seq (i_1, i_2), []) }

%inline var_annot:
    AT i=IDENT { i }

%inline inst_annot(I):
    I a=var_annot* { a }

instruction_d:
    a=inst_annot(I_DROP)  { (I_drop, a) }
  | a=inst_annot(I_DROP) n=NUM { I_drop_n n, a }
  | a=inst_annot(I_DUP) { I_dup, a }
  | a=inst_annot(I_SWAP) { I_swap, a }
  | a=inst_annot(I_DIG) n=NUM { I_dig n, a }
  | a=inst_annot(I_DUG) n=NUM { I_dug n, a }
  | a=inst_annot(I_PUSH) t=typ d=data { I_push (t, d), a }
  | a=inst_annot(I_SOME)  { I_some, a }
  | a=inst_annot(I_NONE) t=typ  { I_none t, a }
  | a=inst_annot(I_UNIT)  { I_unit , a }
  | a=inst_annot(I_IF_NONE) i_1=instruction_block i_2=instruction_block { I_if_none (i_1, i_2), a }
  | a=inst_annot(I_IF_SOME) i_1=instruction_block i_2=instruction_block { I_if_none (i_2, i_1), a }
  | a=inst_annot(I_PAIR)  { I_pair, a }
  | a=inst_annot(I_CAR) { I_car, a }
  | a=inst_annot(I_CDR) { I_cdr, a }
  | a=inst_annot(I_LEFT) t=typ  { I_left t, a }
  | a=inst_annot(I_RIGHT) t=typ { I_right t, a }
  | a=inst_annot(I_IF_LEFT) i_1=instruction_block i_2=instruction_block { I_if_left (i_1, i_2), a }
  | a=inst_annot(I_NIL) t=typ { I_nil t, a }
  | a=inst_annot(I_CONS)  { I_cons, a }
  | a=inst_annot(I_IF_CONS) i_1=instruction_block i_2=instruction_block { I_if_cons (i_1, i_2), a }
  | a=inst_annot(I_SIZE)  { I_size, a }
  | a=inst_annot(I_EMPTY_SET) t=comparable_type { I_empty_set t, a }
  | a=inst_annot(I_EMPTY_MAP) t_1=comparable_type t_2=typ { I_empty_map (t_1, t_2), a }
  | a=inst_annot(I_EMPTY_BIG_MAP) t_1=comparable_type t_2=typ { I_empty_big_map (t_1, t_2), a }
  | a=inst_annot(I_MAP) i=instruction_block { I_map i, a }
  | a=inst_annot(I_ITER) i=instruction_block { I_iter i, a }
  | a=inst_annot(I_MEM) { I_mem, a }
  | a=inst_annot(I_GET) { I_get, a }
  | a=inst_annot(I_UPDATE)  { I_update, a }
  | a=inst_annot(I_IF) i_1=instruction_block i_2=instruction_block  { I_if (i_1, i_2), a }
  | a=inst_annot(I_LOOP) i=instruction_block  { I_loop i, a }
  | a=inst_annot(I_LOOP_LEFT) i=instruction_block { I_loop_left i, a }
  | a=inst_annot(I_LAMBDA) t_1=typ t_2=typ i=instruction_block  { I_lambda (t_1, t_2, i), a }
  | a=inst_annot(I_EXEC)  { I_exec, a }
  | a=inst_annot(I_DIP) i=instruction_block { I_dip i, a }
  | a=inst_annot(I_DIP) n=NUM i=instruction_block { I_dip_n (n, i), a }
  | a=inst_annot(I_FAILWITH)  { I_failwith, a }
  | a=inst_annot(I_CAST) t=typ { I_cast t, a }
  | a=inst_annot(I_RENAME) { I_rename, a }
  | a=inst_annot(I_CONCAT) { I_concat, a }
  | a=inst_annot(I_SLICE) { I_slice, a }
  | a=inst_annot(I_PACK) { I_pack, a }
  | a=inst_annot(I_UNPACK) t=typ { I_unpack t, a }
  | a=inst_annot(I_ADD) { I_add, a }
  | a=inst_annot(I_SUB) { I_sub, a }
  | a=inst_annot(I_MUL) { I_mul, a }
  | a=inst_annot(I_EDIV) { I_ediv, a }
  | a=inst_annot(I_ABS) { I_abs, a }
  | a=inst_annot(I_ISNAT) { I_isnat, a }
  | a=inst_annot(I_INT) { I_int, a }
  | a=inst_annot(I_NEG) { I_neg, a }
  | a=inst_annot(I_LSL) { I_lsl, a }
  | a=inst_annot(I_LSR) { I_lsr, a }
  | a=inst_annot(I_OR) { I_or, a }
  | a=inst_annot(I_AND) { I_and, a }
  | a=inst_annot(I_XOR) { I_xor, a }
  | a=inst_annot(I_NOT) { I_not, a }
  | a=inst_annot(I_COMPARE) { I_compare, a }
  | a=inst_annot(I_EQ) { I_eq, a }
  | a=inst_annot(I_NEQ) { I_neq, a }
  | a=inst_annot(I_LT) { I_lt, a }
  | a=inst_annot(I_GT) { I_gt, a }
  | a=inst_annot(I_LE) { I_le, a }
  | a=inst_annot(I_GE) { I_ge, a }
  | a=inst_annot(I_SELF) { I_self, a }
  | a=inst_annot(I_CONTRACT) t=typ { I_contract t, a }
  | a=inst_annot(I_TRANSFER_TOKENS) { I_transfer_tokens, a }
  | a=inst_annot(I_SET_DELEGATE) { I_set_delegate, a }
  | a=inst_annot(I_CREATE_ACCOUNT) { I_create_account, a }
  | a=inst_annot(I_CREATE_CONTRACT) LB c=program RB { I_create_contract c, a }
  | a=inst_annot(I_IMPLICIT_ACCOUNT) { I_implicit_account, a }
  | a=inst_annot(I_NOW) { I_now, a }
  | a=inst_annot(I_AMOUNT) { I_amount, a }
  | a=inst_annot(I_BALANCE) { I_balance, a }
  | a=inst_annot(I_CHECK_SIGNATURE) { I_check_signature, a }
  | a=inst_annot(I_BLAKE2B) { I_blake2b, a }
  | a=inst_annot(I_SHA256) { I_sha256, a }
  | a=inst_annot(I_SHA512) { I_sha512, a }
  | a=inst_annot(I_HASH_KEY) { I_hash_key, a }
  | a=inst_annot(I_STEPS_TO_QUOTA) { I_steps_to_quota, a }
  | a=inst_annot(I_SOURCE) { I_source, a }
  | a=inst_annot(I_SENDER) { I_sender, a }
  | a=inst_annot(I_ADDRESS) { I_address, a }
  | a=inst_annot(I_CHAIN_ID) { I_chain_id, a }
  | a=inst_annot(I_UNPAIR) { I_unpair, a }

int:
    n=NUM { n }
  | MINUS n=NUM { Z.neg n }

data:
    n=int { D_int n }
  | s=STRING  { D_string s }
  | b=HEX { D_bytes b }
  | UNIT  { D_unit }
  | b=BOOLEAN  { D_bool b }
  | PAIR d_1=data d_2=data  { D_pair (d_1, d_2) }
  | LEFT d=data { D_left d }
  | RIGHT d=data  { D_right d }
  | SOME d=data { D_some d }
  | NONE  { D_none }
  | ELT d_1=data d_2=data { D_elt (d_1, d_2) }
  | d=delimited(LB, separated_nonempty_list(SEMICOLON, data), RB) { D_list d }
  
