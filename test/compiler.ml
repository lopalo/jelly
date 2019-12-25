module A = Alcotest
module Expr = Jelly.Expression
module Obj = Jelly.Object
module Cmp = Jelly.Compiler

let symbol name = Expr.Symbol.Symbol name

let symbol_set = Expr.SymbolSet.of_list

type meta = Jelly.Common.meta

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
       let empty = SymbolSet.empty in
       let m line_number column_number =
         Some
           ({source_name = "expressions.jly"; line_number; column_number}
             : meta)
       in
       Lambda
         { arguments = Fixed [];
           closure_names = empty;
           expressions =
             [ Define {name = symbol ">"; expression = Value Null; meta = m 0 0};
               Define {name = symbol "+"; expression = Value Null; meta = m 1 0};
               Define {name = symbol "y"; expression = Value Null; meta = m 2 0};
               Define
                 {name = symbol "cons"; expression = Value Null; meta = m 3 0};
               Define
                 { name = symbol "make-num-seq";
                   expression =
                     Lambda
                       { arguments = Fixed [symbol "fn"; symbol "n"];
                         closure_names =
                           symbol_set
                             [symbol ">"; symbol "cons"; symbol "make-num-seq"];
                         expressions =
                           [ Define
                               { name = symbol "y";
                                 expression =
                                   Lambda
                                     { arguments = Fixed [symbol "n"];
                                       closure_names = symbol_set [symbol "fn"];
                                       expressions =
                                         [ Application
                                             { expressions =
                                                 [ Identifier
                                                     (symbol "fn", m 7 27);
                                                   Identifier
                                                     (symbol "n", m 7 30) ];
                                               evaluated_expressions = [];
                                               meta = m 7 26 } ];
                                       meta = m 7 14 };
                                 meta = m 7 4 };
                             If
                               { condition =
                                   Application
                                     { expressions =
                                         [ Identifier (symbol ">", m 8 9);
                                           Identifier (symbol "n", m 8 11);
                                           Value (Int 100) ];
                                       evaluated_expressions = [];
                                       meta = m 8 8 };
                                 then_expr = Value Null;
                                 else_expr =
                                   Application
                                     { expressions =
                                         [ Identifier (symbol "cons", m 10 7);
                                           Identifier (symbol "n", m 10 12);
                                           Application
                                             { expressions =
                                                 [ Identifier
                                                     ( symbol "make-num-seq",
                                                       m 10 15 );
                                                   Application
                                                     { expressions =
                                                         [ Identifier
                                                             ( symbol "y",
                                                               m 10 29 );
                                                           Identifier
                                                             ( symbol "n",
                                                               m 10 31 ) ];
                                                       evaluated_expressions =
                                                         [];
                                                       meta = m 10 28 } ];
                                               evaluated_expressions = [];
                                               meta = m 10 14 } ];
                                       evaluated_expressions = [];
                                       meta = m 10 6 } } ];
                         meta = m 6 2 };
                   meta = m 5 0 };
               Define
                 { name = symbol "num-seq";
                   expression =
                     Application
                       { expressions =
                           [ Identifier (symbol "make-num-seq", m 12 17);
                             Lambda
                               { arguments = Fixed [symbol "n"];
                                 closure_names = symbol_set [symbol "+"];
                                 expressions =
                                   [ Application
                                       { expressions =
                                           [ Identifier (symbol "+", m 12 43);
                                             Value (Int 10);
                                             Identifier (symbol "n", m 12 48)
                                           ];
                                         evaluated_expressions = [];
                                         meta = m 12 42 } ];
                                 meta = m 12 30 };
                             Value (Int 0) ];
                         evaluated_expressions = [];
                         meta = m 12 16 };
                   meta = m 12 0 } ];
           meta = None }))
    (Common.compile_test_script "expressions.jly")

let invalid_lambda_arg () =
  check "invalid form error"
    (Error
       (Cmp.InvalidForm
          (Common.obj_of_str "(lambda (q 666) (gg 1 2) (1 2 3) n)")))
    (Common.compile_test_script "wrong-lambda-argument.jly")

let invalid_operator () =
  check "invalid form error"
    (Error (Cmp.InvalidForm (Common.obj_of_str "(5.1 n)")))
    (Common.compile_test_script "wrong-operator.jly")

let undefined_name () =
  check "undefined name error"
    (Error
       (Cmp.UndefinedName
          ( symbol "zzz",
            Some
              { source_name = "undefined-name.jly";
                line_number = 8;
                column_number = 32 } )))
    (Common.compile_test_script "undefined-name.jly")

let redefined_local_name () =
  check "duplicate local definition error"
    (Error
       (Cmp.DuplicateLocalDefinition
          ( symbol "fn",
            Some
              { source_name = "redefined-local-name.jly";
                line_number = 7;
                column_number = 6 } )))
    (Common.compile_test_script "redefined-local-name.jly")

let tests =
  [ A.test_case "expressions" `Quick expressions;
    A.test_case "invalid lambda argument" `Quick invalid_lambda_arg;
    A.test_case "invalid operator" `Quick invalid_operator;
    A.test_case "undefined name" `Quick undefined_name;
    A.test_case "redefined local name" `Quick redefined_local_name ]
