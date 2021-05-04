{
  open Parser
  open Lexing

  exception Lexing_error of string

  let kwd_tbl = [
      "parameter", PARAMETER;
      "storage", STORAGE;
      "code", CODE;
      (* Types *)
      "key", T_KEY; "unit", T_UNIT; "signature", T_SIGNATURE; "option", T_OPTION;
      "list", T_LIST; "set", T_SET; "operation", T_OPERATION; "contract", T_CONTRACT;
      "pair", T_PAIR; "or", T_OR; "lambda", T_LAMBDA; "map", T_MAP; "big_map", T_BIG_MAP;
      "chain_id", T_CHAIN_ID; "int", T_INT; "nat", T_NAT; "string", T_STRING;
      "bytes", T_BYTES; "mutez", T_MUTEZ; "bool", T_BOOL; "key_hash", T_KEY_HASH;
      "timestmp", T_TIMESTAMP; "address", T_ADDRESS;

      (* Data *)
      "Unit", UNIT; "True", BOOLEAN true; "False", BOOLEAN false; "Pair", PAIR;
      "Left", LEFT; "Right", RIGHT; "Some", SOME; "None", NONE; "Elt", ELT;
      
      (* Instructions *)
      "FAILWITH", I_FAILWITH; "IF", I_IF; "LOOP", I_LOOP; "LOOP_LEFT", I_LOOP_LEFT;
      "DIP", I_DIP; "EXEC", I_EXEC; "APPLY", I_APPLY; "DROP", I_DROP; "DUP", I_DUP;
      "SWAP", I_SWAP; "DIG", I_DIG; "DUG", I_DUG; "PUSH", I_PUSH; "UNIT", I_UNIT;
      "LAMBDA", I_LAMBDA; "EQ", I_EQ; "NEQ", I_NEQ; "LT", I_LT; "GT", I_GT; "LE", I_LE;
      "GE", I_GE; "OR", I_OR; "AND", I_AND; "XOR", I_XOR; "NOT", I_NOT; "NEG", I_NEG;
      "ABS", I_ABS; "ISNAT", I_ISNAT; "INT", I_INT; "ADD", I_ADD; "SUB", I_SUB; "MUL", I_MUL;
      "EDIV", I_EDIV; "LSL", I_LSL; "LSR", I_LSR; "COMPARE", I_COMPARE; "CONCAT", I_CONCAT;
      "SLICE", I_SLICE; "SIZE", I_SIZE; "PAIR", I_PAIR; "CAR", I_CAR; "CDR", I_CDR;
      "EMPTY_SET", I_EMPTY_SET; "MEM", I_MEM; "UPDATE", I_UPDATE; "ITER", I_ITER;
      "EMPTY_MAP", I_EMPTY_MAP; "GET", I_GET; "MAP", I_MAP; "EMPTY_BIG_MAP", I_EMPTY_BIG_MAP;
      "SOME", I_SOME; "NONE", I_NONE; "IF_NONE", I_IF_NONE; "LEFT", I_LEFT; "RIGHT", I_RIGHT;
      "IF_LEFT", I_IF_LEFT; "CONS", I_CONS; "NIL", I_NIL; "IF_CONS", I_IF_CONS;
      "CREATE_CONTRACT", I_CREATE_CONTRACT; "TRANSFER_TOKENS", I_TRANSFER_TOKENS;
      "SET_DELEGATE", I_SET_DELEGATE; "BALANCE", I_BALANCE; "ADDRESS", I_ADDRESS;
      "CONTRACT", I_CONTRACT; "SOURCE", I_SOURCE; "SENDER", I_SENDER; "SELF", I_SELF;
      "AMOUNT", I_AMOUNT; "IMPLICIT_ACCOUNT", I_IMPLICIT_ACCOUNT; "NOW", I_NOW;
      "CHAIN_ID", I_CHAIN_ID; "PACK", I_PACK; "UNPACK", I_UNPACK; "HASH_KEY", I_HASH_KEY;
      "BLAKE2B", I_BLAKE2B; "SHA256", I_SHA256; "SHA512", I_SHA512;
      "CHECK_SIGNATURE", I_CHECK_SIGNATURE; "CAST", I_CAST; "UNPAIR", I_UNPAIR;
      "RENAME", I_RENAME; ]

  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let number = digit+
let space = ' ' | '\t' | '\r'
let string_content = "\\\"" | '\r' | '\n' | '\t' | '\b' | "\\\\" | [^ '"' '\\']
let string = '"' string_content* '"'
let new_line = '\n' | "\r\n"
let ident = letter (letter | digit | '_')*
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let comment = '#' [^ '\n']* new_line

rule next_token = parse
  | comment       { new_line lexbuf; next_token lexbuf }
  | new_line      { new_line lexbuf; next_token lexbuf }
  | space+        { next_token lexbuf }
  | string as s   { STRING s }
  | ident as s    { id_or_kwd s }
  | hex as s      { HEX (Hex.to_bytes (`Hex (String.sub s 2 (String.length s - 2)))) }
  | number as s   { NUM (Bigint.of_string s) }
  | ';'           { SEMICOLON }
  | '{'           { LB }
  | '}'           { RB }
  | '('           { LP }
  | ')'           { RP }
  | '-'           { MINUS }
  | '%'           { PERCENT }
  | '@'           { AT }
  | ':'           { COLON }
  | eof           { EOF }
  | _ as c        { raise (Lexing_error ("Illegal character: " ^ String.make 1 c)) }

and comment = parse
  | new_line  { new_line lexbuf; next_token lexbuf }
  | eof       { EOF }
  | _         { comment lexbuf }
