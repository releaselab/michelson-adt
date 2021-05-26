open Alcotest

let test dir parse_micheline parse_program typecheck name =
  let files = Sys.readdir dir in

  let create_test file =
    let parse_f () = parse_micheline (dir ^ file) in
    let test_f () =
      match parse_f () with
      | Ok ast -> (
          try
            let ast, _ = parse_program ast in
            if typecheck ast then check pass "Ok" () ()
            else fail "Type check failed"
          with Michelson.Carthage.Parse.Parse_error e ->
            fail
              (Stdlib.Format.fprintf Stdlib.Format.str_formatter
                 "Parsing error: %s" e;
               Format.flush_str_formatter ()))
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
  (name, tests)

let carthage =
  test "../../../../tests/carthage/" Michelson.Carthage.Parse.parse_program
    Michelson.Carthage.Parse.program_parse Michelson.Carthage.Typer.typecheck
    "parsing carthage"

let edo =
  test "../../../../tests/edo/" Michelson.Edo.Parse.parse_program
    Michelson.Edo.Parse.program_parse
    (fun _ -> true)
    "parsing edo"

let () = run "Michelson parser" [ carthage; edo ]
