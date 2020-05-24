open Printf

let path = Sys.argv.(1)

let () =
  let template file =
    let () = printf "--- %s ---" file in
    let () = print_newline () in
    try
      let f = open_in (path ^ file) in
      let open Michelson in
      let lexbuf = Lexing.from_channel f in
      let _ = Michelson.Parser.start Lexer.next_token lexbuf in
      let () = close_in f in
      Printf.printf "Test %s: OK\n" file
    with e ->
      let msg = Printexc.to_string e in
      let stack = Printexc.get_backtrace () in
      Printf.printf "Test %s: not OK | %s%s\n" file msg stack
  in
  let dir_files = Sys.readdir path in
  Array.iter template dir_files
