open Base

let print_error_position lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Stdlib.Format.sprintf "Line: %d Position: %d" pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_program filename =
  let in_c = Stdio.In_channel.create filename in
  let lexbuf = Lexing.from_channel in_c in
  let adt =
    try Ok (Parser.start Lexer.next_token lexbuf) with
    | Lexer.Lexing_error msg ->
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
  adt
