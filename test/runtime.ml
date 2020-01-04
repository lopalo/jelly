module A = Alcotest
module Obj = Jelly.Object
module RT = Jelly.Runtime

let check = A.(testable Obj.pp Obj.equal |> check)

let lambda_application () =
  check "tuple"
    (Common.obj_of_str "(\"result\" 7 3 17)")
    (Common.execute_str
       "((lambda [x] (define y 3) (list \"result\" x y (+ (* x 2) y))) 7)")

let make_number_sequence () =
  check "list of integers"
    (Common.obj_of_str "(0 10 20 30 40 50 60 70 80 90 100)")
    (Common.execute_test_script "number-sequence.jly")

let closure () =
  let module Symbols = Jelly.Expression.SymbolSet in
  let sym name = Jelly.Expression.Symbol.Symbol name in
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
    (let[@warning "-8"] (Obj.Cons (procedures, _)) =
       Common.execute_test_script "closure.jly"
     in
     List.map scope_names procedures)

let tail_call_elimination () =
  check "integer"
    (Common.obj_of_str "1000002")
    (Common.execute_test_script "tail-call.jly")

let tests =
  [ A.test_case "lambda application" `Quick lambda_application;
    A.test_case "make number sequence" `Quick make_number_sequence;
    A.test_case "closure" `Quick closure;
    A.test_case "tail call elimination" `Slow tail_call_elimination ]
