open Core_kernel

let () =
  let dir = "../../../../tests/carthage/" in
  let files = Sys.readdir dir in
  let open Alcotest in
  let create_test file =
    let parse_f () = Michelson.Carthage.Parse.parse_program (dir ^ file) in
    let test_f () =
      match parse_f () with
      | Ok _ -> check pass "Ok" () ()
      | Error e ->
          fail
            (Stdlib.Format.fprintf Stdlib.Format.str_formatter
               "Parsing error: %a" Error.pp e;
             Format.flush_str_formatter ())
    in
    test_case file `Quick test_f
  in
  let tests = Array.map files ~f:create_test in
  let tests = Array.to_list tests in
  run "Michelson parser" [ ("parsing", tests) ]
