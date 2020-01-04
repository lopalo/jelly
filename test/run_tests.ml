module A = Alcotest

let () =
  A.run "jelly"
    [ ("parser", Parser.tests);
      ("compiler", Compiler.tests);
      ("runtime", Runtime.tests) ]
