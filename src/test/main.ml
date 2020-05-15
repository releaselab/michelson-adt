open Michelson

let path =
  let doc = "Directory where the test files are located." in
  Cmdliner.Arg.(
    required & opt (some string) None & info [ "d" ] ~doc ~docv:"DIR")

let test_empty path =
  try
    let f = open_in (path ^ "empty.tz") in
    let _ = Parser.start Lexer.next_token (Lexing.from_channel f) in
    close_in f
  with e -> Alcotest.failf "Test empty: not OK | %s\n" (Printexc.to_string e)

let test_empty_code path =
  try
    let f = open_in (path ^ "empty_code.tz") in
    let _ = Parser.start Lexer.next_token (Lexing.from_channel f) in
    close_in f
  with e ->
    Alcotest.failf "Test empty_code: not OK | %s\n" (Printexc.to_string e)

let test_loop_left path =
  try
    let f = open_in (path ^ "loop_left.tz") in
    let _ = Parser.start Lexer.next_token (Lexing.from_channel f) in
    close_in f
  with e ->
    Alcotest.failf "Test loop_left: not OK | %s\n" (Printexc.to_string e)

let test_map path =
  try
    let f = open_in (path ^ "map.tz") in
    let _ = Parser.start Lexer.next_token (Lexing.from_channel f) in
    close_in f
  with e -> Alcotest.failf "Test map: not OK | %s\n" (Printexc.to_string e)

let test_i path =
  let tests = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  List.iter
    (fun i ->
      try
        let f = open_in (path ^ "test" ^ string_of_int i ^ ".tz") in
        let _ = Parser.start Lexer.next_token (Lexing.from_channel f) in
        close_in f
      with e ->
        Alcotest.failf "Test %d: not OK | %s\n" i (Printexc.to_string e))
    tests

let () =
  let open Alcotest in
  run_with_args "Cfg" path
    [
      ( "convert",
        [
          ("empty", `Quick, test_empty);
          ("empty code", `Quick, test_empty_code);
          ("i", `Quick, test_i);
          ("loop left", `Quick, test_loop_left);
          ("map", `Quick, test_map);
        ] );
    ]
