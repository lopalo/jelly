module A = Alcotest
open Runtime

(* TODO: load script/core.jly *)
let or_form () = ()

let tests = [A.test_case "'or' form" `Quick or_form]
