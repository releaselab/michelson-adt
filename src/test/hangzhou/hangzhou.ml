open Core

let () =
  let dir = "../../../../../tests/hangzhou/" in
  let files = Sys.readdir dir in
  let open Alcotest in
  let create_test file =
    let open Michelson.Hangzhou.Parser in
    let parse_f () =
      let ast = parse_file (dir ^ file) in
      let p = convert file ast in
      let _ = Michelson.Hangzhou.Typer.type_program p in
      ()
    in
    let test_f () =
      try
        parse_f ();
        check pass "Ok" () ()
      with
      | Failure s -> fail ("Parsing error: " ^ s)
      | Michelson.Hangzhou.Typer.Type_error l as e ->
          Stdio.prerr_endline ("Type error: " ^ Michelson.Common.Loc.to_string l);
          raise e
    in
    test_case file `Quick test_f
  in
  let tests = Array.map files ~f:create_test in
  let tests = Array.to_list tests in
  run "Michelson parser" [ ("parsing", tests) ]
