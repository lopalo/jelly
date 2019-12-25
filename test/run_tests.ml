module A = Alcotest

let () = A.run "jelly" [("parser", Parser.tests); ("compiler", Compiler.tests)]
