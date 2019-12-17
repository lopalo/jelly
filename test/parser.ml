module R = Result
module A = Alcotest
module P = Jelly.Parser

let parse_error =
  A.of_pp
    (fun ppf
         (P.ParseError ({line_number = lnum; column_number = cnum; _}, msg))
         ->
      let msg' = Printf.sprintf "%s at %i %i" msg (succ lnum) (succ cnum) in
      Fmt.string ppf msg')

let parse_result testable = A.result testable parse_error

let test_sentences () =
  A.(list string |> list |> parse_result |> check)
    "parsed sentences"
    (R.Ok
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
    (R.Ok
       (let open Jelly.Object in
       [ Bool false;
         Sym (Symbol "qn");
         Int 1;
         Char '\n';
         Char '\'';
         Cons
           [ Int (-444);
             Int 4;
             Char 'n';
             Cons
               [ Float (-0.33);
                 Sym (Symbol "fun");
                 Str "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
                 Char 'm';
                 Cons [Str "ggg\\3333"];
                 Char ' ' ];
             Str "dd";
             Cons [Str "ddd\"qqq"; Char 'n'; Float 4.5] ];
         Null;
         Str "aaa\nbbb\ncccn\nddd" ]))
    (Common.parse_test_script "syntax.jl")

let test_wrong_lisp () =
  A.(list Common.obj |> parse_result |> check)
    "parsed s-expressions"
    (R.Error
       (ParseError
          ( { input = "#vec\"dd\" (ttt 4.5)]\n";
              offset = 82;
              line_number = 3;
              column_number = 31 },
            "Predicate 'expected character' is not satisfied" )))
    (Common.parse_test_script "wrong-syntax.jl")

let tests =
  [ A.test_case "sentences" `Quick test_sentences;
    A.test_case "lisp" `Quick test_lisp;
    A.test_case "wrong-lisp" `Quick test_wrong_lisp ]
