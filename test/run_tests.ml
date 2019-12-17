module A = Alcotest

let () =
  A.run "jelly" [("parser", Parser.tests)]
