module A = Alcotest
module Expr = Jelly.Expression
module Obj = Jelly.Object
module Cmp = Jelly.Compiler

let sym = Expr.symbol

let symbols = Jelly.Symbol.Set.of_list

let check =
  let open A in
  result
    (testable (Expr.pp Obj.pp) (Expr.equal Obj.equal))
    (testable Cmp.pp_error Cmp.equal_error)
  |> check

let expressions () =
  check "compiled expressions"
    (Ok
       (let open Expr in
       let m line_number column_number =
         Some
           ({source_name = "expressions.jly"; line_number; column_number}
             : Jelly.Common.meta)
       in
       let identifier name line_number column_number =
         Identifier {name = sym name; meta = m line_number column_number}
       in
       Lambda
         { arguments = Fixed [];
           closure_names = Jelly.Core.definitions;
           expressions =
             [ Define {name = sym "y"; expression = Value Null; meta = m 0 0};
               Define
                 { name = sym "make-num-seq";
                   expression =
                     Lambda
                       { arguments = Fixed [sym "fn"; sym "n"];
                         closure_names =
                           symbols [sym ">"; sym "cons"; sym "make-num-seq"];
                         expressions =
                           [ Define
                               { name = sym "y";
                                 expression =
                                   Lambda
                                     { arguments = Fixed [sym "n"];
                                       closure_names = symbols [sym "fn"];
                                       expressions =
                                         [ Application
                                             { expressions =
                                                 [ identifier "fn" 4 27;
                                                   identifier "n" 4 30 ];
                                               computed_values = [];
                                               meta = m 4 26 } ];
                                       meta = m 4 14 };
                                 meta = m 4 4 };
                             If
                               { condition =
                                   Application
                                     { expressions =
                                         [ identifier ">" 5 9;
                                           identifier "n" 5 11;
                                           Value (Int 100) ];
                                       computed_values = [];
                                       meta = m 5 8 };
                                 then_expr = Value (Obj.list [Int 1; Int 2]);
                                 else_expr =
                                   Application
                                     { expressions =
                                         [ identifier "cons" 7 7;
                                           identifier "n" 7 12;
                                           Application
                                             { expressions =
                                                 [ identifier "make-num-seq" 7
                                                     15;
                                                   Application
                                                     { expressions =
                                                         [ identifier "y" 7 29;
                                                           identifier "n" 7 31
                                                         ];
                                                       computed_values = [];
                                                       meta = m 7 28 } ];
                                               computed_values = [];
                                               meta = m 7 14 } ];
                                       computed_values = [];
                                       meta = m 7 6 } } ];
                         meta = m 3 2 };
                   meta = m 2 0 };
               Define
                 { name = sym "num-seq";
                   expression =
                     Application
                       { expressions =
                           [ identifier "make-num-seq" 9 17;
                             Lambda
                               { arguments = Fixed [sym "n"];
                                 closure_names = symbols [sym "+"];
                                 expressions =
                                   [ Application
                                       { expressions =
                                           [ identifier "+" 9 43;
                                             Value (Int 10);
                                             identifier "n" 9 48 ];
                                         computed_values = [];
                                         meta = m 9 42 } ];
                                 meta = m 9 30 };
                             Value (Int 0) ];
                         computed_values = [];
                         meta = m 9 16 };
                   meta = m 9 0 } ];
           meta = None }))
    (Common.compile_test_script "expressions.jly")

let invalid_lambda_arg () =
  check "invalid form error"
    (Error
       (`InvalidForm (Common.obj_of_str "(lambda (q 666) (gg 1 2) (1 2 3) n)")))
    (Common.compile_test_script "error/wrong-lambda-argument.jly")

let invalid_operator () =
  check "invalid form error"
    (Error (`InvalidForm (Common.obj_of_str "(5.1 n)")))
    (Common.compile_test_script "error/wrong-operator.jly")

let misplaced_define () =
  check "invalid form error"
    (Error (`InvalidForm (Common.obj_of_str "(define y (lambda [n] (fn n)))")))
    (Common.compile_test_script "error/misplaced-define.jly")

let undefined_name () =
  check "undefined name error"
    (Error
       (`UndefinedName
         ( sym "zzz",
           Some
             { source_name = "error/undefined-name.jly";
               line_number = 4;
               column_number = 32 } )))
    (Common.compile_test_script "error/undefined-name.jly")

let redefined_local_name () =
  check "duplicate local definition error"
    (Error
       (`DuplicateDefinition
         ( sym "fn",
           Some
             { source_name = "error/redefined-local-name.jly";
               line_number = 5;
               column_number = 4 } )))
    (Common.compile_test_script "error/redefined-local-name.jly")

let tests =
  [ A.test_case "expressions" `Quick expressions;
    A.test_case "invalid lambda argument" `Quick invalid_lambda_arg;
    A.test_case "invalid operator" `Quick invalid_operator;
    A.test_case "misplaced define" `Quick misplaced_define;
    A.test_case "undefined name" `Quick undefined_name;
    A.test_case "redefined local name" `Quick redefined_local_name ]
