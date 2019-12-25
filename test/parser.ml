module A = Alcotest
module P = Jelly.Parser

let error = A.testable P.pp_error P.equal_error

let parse_result testable = A.result testable error

let test_sentences () =
  A.(list string |> list |> parse_result |> check)
    "parsed sentences"
    (Ok
       [ ["Jelly"; "is"; "a"; "programming"; "language"; "so"; "is"; "Python"];
         ["Sentence"; "2"];
         ["Sentence"; "xxxx"; "3"];
         ["Sentence"; "4"; "zzzzz"];
         ["Sentence"; "5"] ])
    (P.parse_sentences
       " Jelly is\n\
       \    a  programming language\n\
        , so is Python. Sentence 2.\n\
        Sentence   \n\
       \ xxxx\n\
       \      3.  Sentence 4 zzzzz \n\
        . Sentence 5  ")

let test_lisp () =
  A.(list Common.obj |> parse_result |> check)
    "parsed s-expressions"
    (Ok
       (let open Jelly.Object in
       [ Bool false;
         symbol "qn";
         Int 1;
         Char '\n';
         Char '\'';
         list
           [ Int (-444);
             Int 4;
             Char 'n';
             list
               [ Float (-0.33);
                 symbol "fun";
                 Str "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
                 Char 'm';
                 list [Str "ggg\\3333"];
                 Char ' ' ];
             Str "dd";
             list [Str "ddd\"qqq"; Char 'n'; Float 4.5] ];
         Null;
         Str "aaa\nbbb\ncccn\nddd" ]))
    (Common.parse_test_script "syntax.jly")

let test_wrong_lisp () =
  A.(list Common.obj |> parse_result |> check)
    "parse error"
    (Error
       (ParseError
          ( { input = "#vec\"dd\" (ttt 4.5)]\n";
              offset = 82;
              source_name = "wrong-syntax.jly";
              line_number = 3;
              column_number = 31 },
            "Predicate 'expected character' is not satisfied" )))
    (Common.parse_test_script "wrong-syntax.jly")

let tests =
  [ A.test_case "sentences" `Quick test_sentences;
    A.test_case "lisp" `Quick test_lisp;
    A.test_case "wrong-lisp" `Quick test_wrong_lisp ]
