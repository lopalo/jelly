module A = Alcotest
module Obj = Jelly.Object
module RT = Jelly.Runtime

let sym name = Jelly.Expression.symbol name

let check =
  let open A in
  result (testable Obj.pp Obj.equal) (testable RT.pp_error RT.equal_error)
  |> check

let lambda_application () =
  check "tuple"
    (Ok (Common.obj_of_str "(\"result\" 7 3 17)"))
    (Common.execute_str
       "((lambda [x] (define y 3) (list \"result\" x y (+ (* x 2) y))) 7)")

let make_number_sequence () =
  check "list of integers"
    (Ok (Common.obj_of_str "(0 10 20 30 40 50 60 70 80 90 100)"))
    (Common.execute_test_script "number-sequence.jly")

let closure () =
  let module Symbols = Jelly.Symbol.Set in
  let symbols = Symbols.of_list in
  let scope_names = function
    | Obj.Procedure (Closure {scope; _}) -> RT.scope_to_definitions scope
    | _ -> failwith "is not a closure"
  in
  A.(check @@ list @@ testable Symbols.pp Symbols.equal)
    "captured names"
    [ symbols [sym "list"; sym "-"; sym "foo"; sym "bar"; sym "x"];
      symbols [sym "list"; sym "foo"; sym "b"];
      symbols [sym "list"; sym "bar"; sym "-"; sym "x"] ]
    (let[@warning "-8"] (Ok (Obj.Cons (procedures, _))) =
       Common.execute_test_script "closure.jly"
     in
     List.map scope_names procedures)

let rt_error (error_context : RT.error_context) =
  Error (`RuntimeError error_context)

let tail_call_elimination () =
  check "error"
    (rt_error
       { error = "100002";
         stack_size = 0;
         stack_trace =
           [ { source_name = "tail-call.jly";
               line_number = 15;
               column_number = 10 } ] })
    (Common.execute_test_script "tail-call.jly")

let stack_trace () =
  check "error"
    (rt_error
       { error = "end";
         stack_size = 15;
         stack_trace =
           (let m line_number column_number =
              ({source_name = "stack-trace.jly"; line_number; column_number}
                : Jelly.Common.meta)
            in
            [ m 4 11;
              m 4 6;
              m 1 2;
              m 5 6;
              m 1 2;
              m 5 6;
              m 1 2;
              m 5 6;
              m 1 2;
              m 5 6;
              m 1 2;
              m 5 6;
              m 1 2;
              m 5 6;
              m 1 2 ]) })
    (Common.execute_test_script "stack-trace.jly")

let bad_arg () =
  check "error"
    (rt_error
       { error = "bad-arg.cons.2: a";
         stack_size = 0;
         stack_trace =
           [{source_name = "no source"; line_number = 0; column_number = 27}] })
    (Common.execute_str "((lambda [x] (define y 'a) (cons x y)) 7)")

let wrong_lambda_arguments_number () =
  check "error"
    (rt_error
       { error = "Closure takes 1 arguments";
         stack_size = 0;
         stack_trace =
           [{source_name = "no source"; line_number = 0; column_number = 0}] })
    (Common.execute_str "((lambda [x] (define y 'a) (cons x y)) 7 2)")

let wrong_function_arguments_number () =
  check "error"
    (rt_error
       { error = "Function takes 2 arguments";
         stack_size = 0;
         stack_trace =
           [{source_name = "no source"; line_number = 0; column_number = 27}] })
    (Common.execute_str "((lambda [x] (define y 'a) (cons x)) 7)")

let non_procedure_application () =
  check "error"
    (rt_error
       { error = "a is not a procedure";
         stack_size = 0;
         stack_trace =
           [{source_name = "no source"; line_number = 0; column_number = 27}] })
    (Common.execute_str "((lambda [x] (define y 'a) (y x)) 7)")

let error_handler () =
  check "error"
    (rt_error
       { error = "bad-arg.display: (1 x y \"bad-args.+: 2, x\")";
         stack_size = 1;
         stack_trace =
           [ { source_name = "error-handler.jly";
               line_number = 6;
               column_number = 4 };
             { source_name = "error-handler.jly";
               line_number = 1;
               column_number = 2 } ] })
    (Common.execute_test_script "error-handler.jly")

let redefined_top_level_name () =
  check "error"
    (Error
       (`DuplicateDefinition
         ( sym "x",
           Some {source_name = "no source"; line_number = 0; column_number = 22}
         )))
    (Common.execute_str "(begin (define x 333) (define x 777))")

let tests =
  [ A.test_case "lambda application" `Quick lambda_application;
    A.test_case "make number sequence" `Quick make_number_sequence;
    A.test_case "closure" `Quick closure;
    A.test_case "tail call elimination" `Quick tail_call_elimination;
    A.test_case "stack trace" `Quick stack_trace;
    A.test_case "bad arg" `Quick bad_arg;
    A.test_case "wrong lambda arguments number" `Quick
      wrong_lambda_arguments_number;
    A.test_case "wrong function arguments number" `Quick
      wrong_function_arguments_number;
    A.test_case "non procedure application" `Quick non_procedure_application;
    A.test_case "error handler" `Quick error_handler;
    A.test_case "redefined top level name" `Quick redefined_top_level_name ]
