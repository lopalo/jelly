module P = Jelly.Parser
module Cmp = Jelly.Compiler
module RT = Jelly.Runtime

let parse_test_script file_name =
  "../../../script/test/" ^ file_name
  |> Jelly.Common.read_file
  |> P.parse_lisp ~source_name:file_name

let compile_test_script file_name =
  match parse_test_script file_name with
  | Ok r -> Cmp.compile_top_level r
  | Error e -> failwith @@ P.show_error e

let execute_test_script file_name =
  match compile_test_script file_name with
  | Ok r -> RT.execute_top_level r
  | Error e -> failwith @@ Cmp.show_error e

let obj_of_str str =
  match P.parse_lisp str with
  | Ok [obj] -> obj
  | _ -> failwith @@ "Cannot create single object from string: " ^ str

let execute_str str =
  match obj_of_str str |> (fun x -> [x]) |> Cmp.compile_top_level with
  | Ok r -> RT.execute_top_level r
  | _ -> failwith @@ "Cannot compile string: " ^ str

let obj =
  let open Jelly.Object in
  Alcotest.testable pp equal
