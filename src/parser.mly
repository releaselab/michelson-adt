
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
  
  let instruction_list_to_seq =
    List.fold_left (fun acc i -> create_node ~loc:acc.loc (Inst (I_seq (acc, i)))) (create_node (Inst I_noop))

  let position_of_lexbuf_pos p =
    let open Location in
    let open Lexing in
    Pos { col = p.pos_cnum; lin = p.pos_lnum }

  let create_node p = create_node ~loc:(position_of_lexbuf_pos p)
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
%token I_STEPS_TO_QUOTA I_SOURCE I_SENDER I_ADDRESS I_CHAIN_ID T_KEY T_UNIT
%token T_SIGNATURE T_OPTION T_LIST T_SET T_OPERATION T_CONTRACT T_PAIR T_OR
%token T_LAMBDA T_MAP T_BIG_MAP T_CHAIN_ID T_INT T_NAT T_STRING T_BYTES
%token T_MUTEZ T_BOOL T_KEY_HASH T_TIMESTAMP T_ADDRESS I_IF_SOME
%token EOF

%start <Adt.program> start

%%

start:
  p=program EOF { p }

program:
    CODE LB code=instruction RB SEMICOLON STORAGE storage=storage SEMICOLON PARAMETER param=parameter SEMICOLON?
  | CODE LB code=instruction RB SEMICOLON PARAMETER param=parameter SEMICOLON STORAGE storage=storage SEMICOLON?
  | STORAGE storage=storage SEMICOLON CODE LB code=instruction RB SEMICOLON PARAMETER param=parameter SEMICOLON?
  | STORAGE storage=storage SEMICOLON PARAMETER param=parameter SEMICOLON CODE LB code=instruction RB SEMICOLON?
  | PARAMETER param=parameter SEMICOLON CODE LB code=instruction RB SEMICOLON STORAGE storage=storage SEMICOLON?
  | PARAMETER param=parameter SEMICOLON STORAGE storage=storage SEMICOLON CODE LB code=instruction RB SEMICOLON?  { { param; storage; code; } }

storage:
    t=typ { t }

parameter:
    t=typ { t }

typ:
    t=typ_d     { create_node $symbolstartpos (Type t) }
  | LP t=typ RP { t }

%inline typ_d:
    t=comparable_type                                   { T_comparable t }
  | T_KEY                                               { T_key }
  | T_UNIT                                              { T_unit }
  | T_SIGNATURE                                         { T_signature }
  | T_OPTION t=typ                                      { T_option t }
  | T_LIST t=typ                                        { T_list t }
  | T_SET t=comparable_type                             { T_set t }
  | T_OPERATION                                         { T_operation }
  | T_CONTRACT t=typ                                    { T_contract t }
  | T_PAIR LP t_1=typ RP LP t_2=typ RP                  { T_pair (t_1, t_2) }
  | T_OR LP t_1=typ RP LP t_2=typ RP                    { T_or (t_1, t_2) }
  | T_LAMBDA LP t_1=typ RP LP t_2=typ RP                { T_lambda (t_1, t_2) }
  | T_MAP LP t_1=comparable_type RP LP t_2=typ RP       { T_map (t_1, t_2) }
  | T_BIG_MAP LP t_1=comparable_type RP LP t_2=typ RP   { T_big_map (t_1, t_2) }
  | T_CHAIN_ID                                          { T_chain_id }

comparable_type:
    t=comparable_type_d     { create_node $symbolstartpos (Comparable_type t) }

%inline comparable_type_d:
    T_INT                           { T_int }
  | T_NAT                           { T_nat }
  | T_STRING                        { T_string }
  | T_BYTES                         { T_bytes }
  | T_MUTEZ                         { T_mutez }
  | T_BOOL                          { T_bool }
  | T_KEY_HASH                      { T_key_hash }
  | T_TIMESTAMP                     { T_timestamp }
  | T_ADDRESS                       { T_address }

instruction:
    il=separated_list(SEMICOLON, instruction_n) { instruction_list_to_seq il }

%inline instruction_n:
    i=instruction_d { create_node $symbolstartpos (Inst i) }

%inline instruction_d:
    I_DROP  { I_drop }
  | I_DROP n=NUM { I_drop_n n }
  | I_DUP { I_dup }
  | I_SWAP { I_swap }
  | I_DIG n=NUM { I_dig n }
  | I_DUG n=NUM { I_dug n }
  | I_PUSH t=typ d=data { I_push (t, d) }
  | I_SOME  { I_some }
  | I_NONE t=typ  { I_none t }
  | I_UNIT  { I_unit  }
  | I_IF_NONE LB i_1=instruction RB LB i_2=instruction RB { I_if_none (i_1, i_2) }
  | I_IF_SOME LB i_1=instruction RB LB i_2=instruction RB { I_if_none (i_2, i_1) }
  | I_PAIR  { I_pair }
  | I_CAR { I_car }
  | I_CDR { I_cdr }
  | I_LEFT t=typ  { I_left t }
  | I_RIGHT t=typ { I_right t }
  | I_IF_LEFT LB i_1=instruction RB LB i_2=instruction RB { I_if_left (i_1, i_2) }
  | I_NIL t=typ { I_nil t }
  | I_CONS  { I_cons }
  | I_IF_CONS LB i_1=instruction RB LB i_2=instruction RB { I_if_cons (i_1, i_2) }
  | I_SIZE  { I_size }
  | I_EMPTY_SET t=comparable_type { I_empty_set t }
  | I_EMPTY_MAP t_1=comparable_type t_2=typ { I_empty_map (t_1, t_2) }
  | I_EMPTY_BIG_MAP t_1=comparable_type t_2=typ { I_empty_big_map (t_1, t_2) }
  | I_MAP LB i=instruction RB { I_map i }
  | I_ITER LB i=instruction RB  { I_iter i }
  | I_MEM { I_mem }
  | I_GET { I_get }
  | I_UPDATE  { I_update }
  | I_IF LB i_1=instruction RB LB i_2=instruction RB  { I_if (i_1, i_2) }
  | I_LOOP LB i=instruction RB  { I_loop i }
  | I_LOOP_LEFT LB i=instruction RB { I_loop_left i }
  | I_LAMBDA t_1=typ t_2=typ LB i=instruction RB  { I_lambda (t_1, t_2, i)}
  | I_EXEC  { I_exec }
  | I_DIP LB i=instruction RB { I_dip i }
  | I_DIP n=NUM LB i=instruction RB { I_dip_n (n, i) }
  | I_FAILWITH  { I_failwith }
  | I_CAST  { I_cast }
  | I_RENAME { I_rename }
  | I_CONCAT { I_concat }
  | I_SLICE { I_slice }
  | I_PACK { I_pack }
  | I_UNPACK t=typ { I_unpack t }
  | I_ADD { I_add }
  | I_SUB { I_sub }
  | I_MUL { I_mul }
  | I_EDIV { I_ediv }
  | I_ABS { I_abs }
  | I_ISNAT { I_isnat }
  | I_INT { I_int }
  | I_NEG { I_neg }
  | I_LSL { I_lsl }
  | I_LSR { I_lsr }
  | I_OR { I_or }
  | I_AND { I_and }
  | I_XOR { I_xor }
  | I_NOT { I_not }
  | I_COMPARE { I_compare }
  | I_EQ { I_eq }
  | I_NEQ { I_neq }
  | I_LT { I_lt }
  | I_GT { I_gt }
  | I_LE { I_le }
  | I_GE { I_ge }
  | I_SELF { I_self }
  | I_CONTRACT t=typ { I_contract t }
  | I_TRANSFER_TOKENS { I_transfer_tokens }
  | I_SET_DELEGATE { I_set_delegate }
  | I_CREATE_ACCOUNT { I_create_account }
  | I_CREATE_CONTRACT LB i=instruction RB { I_create_contract i }
  | I_IMPLICIT_ACCOUNT { I_implicit_account }
  | I_NOW { I_now }
  | I_AMOUNT { I_amount }
  | I_BALANCE { I_balance }
  | I_CHECK_SIGNATURE { I_check_signature }
  | I_BLAKE2B { I_blake2b }
  | I_SHA256 { I_sha256 }
  | I_SHA512 { I_sha512 }
  | I_HASH_KEY { I_hash_key }
  | I_STEPS_TO_QUOTA { I_steps_to_quota }
  | I_SOURCE { I_source }
  | I_SENDER { I_sender }
  | I_ADDRESS { I_address }
  | I_CHAIN_ID { I_chain_id }

int:
    n=NUM { n }
  | MINUS n=NUM { Z.neg n }

data:
  d=data_d  { create_node $symbolstartpos (Data d) }

%inline data_d:
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
  | LB d=separated_nonempty_list(SEMICOLON, elt) RB { D_map d }
  | LB d=separated_nonempty_list(SEMICOLON, data) RB { D_set d }
  /* | i=instruction { D_instruction i } */

%inline elt:
  ELT d_1=data d_2=data { d_1, d_2 }
