module P = Jelly.Parser
module Cmp = Jelly.Compiler
module RT = Jelly.Runtime

let parse_test_script file_name =
  "../../../script/test/" ^ file_name
  |> Jelly.Common.read_file
  |> P.parse_lisp ~source_name:file_name

let load_core_script () =
  match
    "../../../script/core.jly" |> Jelly.Common.read_file
    |> P.parse_lisp ~source_name:"core.jly"
  with
  | Ok r -> r
  | Error e -> failwith @@ P.show_error e

let compile_test_script file_name =
  match parse_test_script file_name with
  | Ok r -> Cmp.compile_top_level_lambda r
  | Error e -> failwith @@ P.show_error e

let execute_test_script ?(core = false) file_name =
  let core_objs = if core then load_core_script () else [] in
  match parse_test_script file_name with
  | Ok r -> RT.execute_top_level (core_objs @ r)
  | Error e -> failwith @@ P.show_error e

let obj_of_str str =
  match P.parse_lisp str with
  | Ok [obj] -> obj
  | Ok _ -> failwith @@ "Cannot create single object from string: " ^ str
  | Error e -> failwith @@ P.show_error e

let obj_of_test_script file_name =
  match parse_test_script file_name with
  | Ok [obj] -> obj
  | Ok _ ->
      failwith @@ "Cannot create single object from test script: " ^ file_name
  | Error e -> failwith @@ P.show_error e

let execute_str ?(core = false) str =
  let core_objs = if core then load_core_script () else [] in
  let obj = obj_of_str str in
  RT.execute_top_level (core_objs @ [obj])

let obj =
  let open Jelly.Object in
  Alcotest.testable pp equal
