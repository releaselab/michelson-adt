{
  open Parser
  open Lexing

  exception Lexical_error of string

  let pos = Loc.loc_of_lexbuf_positions
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let number = '-'? digit+
let space = ' ' | '\t' | '\r'
let string_content = "\\\"" | '\r' | '\n' | '\t' | '\b' | "\\\\" | [^ '"' ]
let string = '"' string_content* '"'
let new_line = '\n' | "\r\n"
let ident = (letter | digit | '_' | '.')+
let hex = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let comment = [^ '\n']* new_line
let annot = ('%' | '@' | ':') ident*

rule next_token = parse
  | "/*@"
      { spec 0 (Buffer.create 100) lexbuf }
  | "/*"
      { comment 0 lexbuf }
  | "#@" (comment as c)
      { new_line lexbuf; SPEC c }
  | '#' comment
      { new_line lexbuf; next_token lexbuf}
  | new_line
      { new_line lexbuf; next_token lexbuf }
  | space+
      { next_token lexbuf }               
  | string as s
      { STRING s }
  | annot as s    { ANNOT s }
  | ident as s    { IDENT s }
  | hex as s      { HEX (Hex.to_bytes (`Hex (String.sub s 2 (String.length s - 2)))) }
  | number as s   { NUM (Bigint.of_string s) }
  | ';'           { SEMICOLON }
  | '{'           { LB }
  | '}'           { RB }
  | '('           { LP }
  | ')'           { RP }
  | eof           { EOF }
  | _ as c        { raise (Lexical_error ("Illegal character: " ^ String.make 1 c)) }

and spec counter buf = parse
  | "/*" as c
      { Buffer.add_string buf c;
        spec (counter + 1) buf lexbuf }
  | "*/"
      { if counter = 0 then 
          SPEC (Buffer.contents buf)
        else
          spec (counter - 1) buf lexbuf }
  | new_line as n
      { Buffer.add_string buf n; new_line lexbuf; spec counter buf lexbuf }
  | eof
      { raise (Lexical_error "unterminated comment") }
  | _ as c
      { Buffer.add_char buf c; spec counter buf lexbuf }
  

and comment counter = parse
  | "/*"
      { comment (counter + 1) lexbuf }
  | "*/"
      { if counter = 0 then 
          next_token lexbuf 
        else
          comment (counter - 1) lexbuf }
  | new_line
      { new_line lexbuf; comment counter lexbuf }
  | eof
      { raise (Lexical_error "unterminated comment") }
  | _
      { comment counter lexbuf }
