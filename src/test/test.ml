open Alcotest

let carthage =
  let dir = "../../../../tests/carthage/" in
  let files = Sys.readdir dir in
  let create_test file =
    let parse_f () = Michelson.Carthage.Parse.parse_program (dir ^ file) in
    let test_f () =
      match parse_f () with
      | Ok _ -> check pass "Ok" () ()
      | Error e ->
          fail
            (Stdlib.Format.fprintf Stdlib.Format.str_formatter
               "Parsing error: %a" Base.Error.pp e;
             Format.flush_str_formatter ())
    in
    test_case file `Quick test_f
  in
  let tests = Array.map create_test files in
  let tests = Array.to_list tests in
  ("parsing carthage", tests)

let edo =
  let dir = "../../../../tests/edo/" in
  let files = Sys.readdir dir in
  let create_test file =
    let parse_f () = Michelson.Edo.Parse.parse_program (dir ^ file) in
    let test_f () =
      match parse_f () with
      | Ok _ -> check pass "Ok" () ()
      | Error e ->
          fail
            (Stdlib.Format.fprintf Stdlib.Format.str_formatter
               "Parsing error: %a" Base.Error.pp e;
             Format.flush_str_formatter ())
    in
    test_case file `Quick test_f
  in
  let tests = Array.map create_test files in
  let tests = Array.to_list tests in
  ("parsing edo", tests)

let () = run "Michelson parser" [ carthage; edo ]
