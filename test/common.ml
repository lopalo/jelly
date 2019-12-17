let parse_test_script file_name =
  "../../../script/test/" ^ file_name
  |> Jelly.Common.read_file |> Jelly.Parser.parse_lisp

let obj =
  let open Jelly.Object in
  Alcotest.testable formatter is_equal
