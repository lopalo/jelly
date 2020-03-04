module A = Alcotest
open Runtime

let or_form_result_1 () =
  check "last result"
    (Ok (Common.obj_of_str "0"))
    (Common.execute_str ~core:true "(or false () ((lambda [] false)) 0)")

let or_form_result_2 () =
  check "short-circuit evaluation"
    (Ok (Common.obj_of_str "7"))
    (Common.execute_str ~core:true
       "(or false 7 ((lambda [] (fail \"foo\") true)) 0)")

let or_form_expansion () =
  check "expanded form"
    (Ok (Common.obj_of_test_script "expanded-or.jly"))
    (Common.execute_str ~core:true "(expand-syntax '(or 11 22 33 44 55 66))")

let let_star_form_expansion () =
  check "expanded form"
    (Ok (Common.obj_of_test_script "expanded-let-star.jly"))
    (Common.execute_test_script ~core:true "let-star.jly")

let redefined_top_level_name_1 () =
  check "error"
    (Error
       (`DuplicateDefinition
         ( sym "x",
           Some {source_name = "no source"; line_number = 0; column_number = 29}
         )))
    (Common.execute_str "(begin (define-syntax x 333) (define x 777))")

let redefined_top_level_name_2 () =
  check "error"
    (Error
       (`DuplicateDefinition
         ( sym "x",
           Some {source_name = "no source"; line_number = 0; column_number = 22}
         )))
    (Common.execute_str "(begin (define x 333) (define-syntax x 777))")

let redefined_top_level_name_3 () =
  check "error"
    (Error
       (`DuplicateDefinition
         ( sym "x",
           Some {source_name = "no source"; line_number = 0; column_number = 29}
         )))
    (Common.execute_str "(begin (define-syntax x 333) (define-syntax x 777))")

let syntax_expansion_error () =
  check "error"
    (Error
       (`SyntaxExpansionError
         { error = "foo";
           stack_size = 2;
           stack_trace =
             [ { source_name = "error/syntax-expansion-error.jly";
                 line_number = 5;
                 column_number = 6 };
               { source_name = "error/syntax-expansion-error.jly";
                 line_number = 4;
                 column_number = 8 };
               { source_name = "error/syntax-expansion-error.jly";
                 line_number = 4;
                 column_number = 8 } ] }))
    (Common.execute_test_script ~core:true "error/syntax-expansion-error.jly")

let nested_syntax_expansion_error () =
  check "error"
    (rt_error
       { error = "foo";
         stack_size = 4;
         stack_trace =
           [ { source_name = "error/nested-syntax-expansion-error.jly";
               line_number = 5;
               column_number = 6 };
             { source_name = "error/nested-syntax-expansion-error.jly";
               line_number = 4;
               column_number = 8 };
             { source_name = "error/nested-syntax-expansion-error.jly";
               line_number = 4;
               column_number = 8 };
             { source_name = "error/nested-syntax-expansion-error.jly";
               line_number = 15;
               column_number = 4 };
             { source_name = "error/nested-syntax-expansion-error.jly";
               line_number = 14;
               column_number = 2 };
             { source_name = "error/nested-syntax-expansion-error.jly";
               line_number = 22;
               column_number = 2 } ] })
    (Common.execute_test_script ~core:true
       "error/nested-syntax-expansion-error.jly")

let tests =
  [ A.test_case "'or' form result 1" `Quick or_form_result_1;
    A.test_case "'or' form result 2" `Quick or_form_result_2;
    A.test_case "'or' form expansion" `Quick or_form_expansion;
    A.test_case "'let*' form expansion" `Quick let_star_form_expansion;
    A.test_case "redefined top level name 1" `Quick redefined_top_level_name_1;
    A.test_case "redefined top level name 2" `Quick redefined_top_level_name_2;
    A.test_case "redefined top level name 2" `Quick redefined_top_level_name_3;
    A.test_case "syntax expansion error" `Quick syntax_expansion_error;
    A.test_case "nested syntax expansion error" `Quick
      nested_syntax_expansion_error ]
